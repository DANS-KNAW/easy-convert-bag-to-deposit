/**
 * Copyright (C) 2020 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package nl.knaw.dans.easy.bag2deposit.collections

import better.files.File
import cats.instances.list._
import cats.instances.try_._
import cats.syntax.traverse._
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import nl.knaw.dans.lib.error.TryExtensions
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.csv.CSVFormat.RFC4180
import org.apache.commons.csv.{ CSVFormat, CSVParser, CSVRecord }
import resource.managed

import java.nio.charset.{ Charset, StandardCharsets }
import scala.collection.JavaConverters.iterableAsScalaIterableConverter
import scala.util.Try
import scala.xml.Elem

object Collections extends DebugEnhancedLogging {

  private val browser = JsoupBrowser()
  private val resolver: Resolver = Resolver()

  private def parseCsv(file: File, format: CSVFormat): Try[Iterable[CSVRecord]] = {
    managed(CSVParser.parse(
      file.toJava,
      Charset.forName("UTF-8"),
      format
        .withDelimiter(',')
        .withRecordSeparator('\n')
        .withSkipHeaderRecord()
    )).map(_.asScala.filter(_.asScala.nonEmpty))
      .tried
  }

  private val skosCsvFormat = RFC4180.withHeader("URI", "prefLabel", "definition", "broader", "notation")
  private val collectionCsvFormat = RFC4180.withHeader("name", "ids", "type", "comment")

  private def parseCollectionRecord(r: CSVRecord) = r.get("name").trim -> r.get("ids")

  private def parseSkosRecord(r: CSVRecord) = {
    r.get("definition") ->
        <ddm:inCollection
           schemeURI="http://easy.dans.knaw.nl/vocabularies/collecties"
           valueURI={ r.get("URI") }
           subjectScheme="DANS Collection"
        >{ r.get("prefLabel") }</ddm:inCollection>
  }

  def getCollectionsMap(cfgPath: File, maybeFedoraProvider: Option[FedoraProvider]): Map[String, Elem] = {
    val result: Map[String, Elem] = maybeFedoraProvider
      .map { provider =>
        memberDatasetIdToInCollection(collectionDatasetIdToInCollection(cfgPath), provider)
      }
      .getOrElse {
        logger.info(s"No <inCollection> added to DDM, no fedora was configured")
        Map.empty
      }
    result.foreach { case (datasetId, inCollection: Elem) =>
      val x = (inCollection \@ "valueURI").replaceAll(".*/", "")
      logger.info(s"$datasetId $x")
    }
    result
  }

  def collectionDatasetIdToInCollection(cfgDir: File): Seq[(String, Elem)] = {

    val skosMap = parseCsv(cfgDir / "excel2skos-collecties.csv", skosCsvFormat)
      .unsafeGetOrThrow
      .map(parseSkosRecord).toMap

    def inCollectionElem(name: String) = {
      skosMap.getOrElse(
        name,
        <notImplemented>{ s"$name not found in collections skos" }</notImplemented>
      )
    }

    parseCsv(cfgDir / "ThemathischeCollecties.csv", collectionCsvFormat)
      .unsafeGetOrThrow
      .map(parseCollectionRecord)
      .toList
      .filter(_._2.startsWith("easy-dataset:"))
      .flatMap { case (name, datasetIds) =>
        datasetIds.split(",").toSeq
          .map(_ -> inCollectionElem(name))
      }
  }

  def memberDatasetIdToInCollection(collectionDatasetIdToInCollection: Seq[(String, Elem)], fedoraProvider: FedoraProvider): Map[String, Elem] = {

    collectionDatasetIdToInCollection
      .flatMap { case (datasetId, inCollection) =>
        membersOf(datasetId, fedoraProvider)
          .unsafeGetOrThrow
          .map(_ -> inCollection)
      }.toMap
  }

  private def membersOf(datasetId: String, fedoraProvider: FedoraProvider): Try[Seq[String]] = {
    for {
      maybeJumpoffId <- jumpoff(datasetId, fedoraProvider)
      jumpoffId = maybeJumpoffId.getOrElse(throw new Exception(s"no jumpoff for $datasetId"))
      doc <- fedoraProvider
        .disseminateDatastream(jumpoffId, "HTML_MU")
        .map(browser.parseInputStream(_, StandardCharsets.UTF_8.name())).tried
      items = doc >> elementList("a")
      hrefs = items
        .withFilter(_.hasAttr("href"))
        .map(_.attr("href"))
        .sortBy(identity)
        .distinct
      // (?s) matches multiline values like https://github.com/DANS-KNAW/easy-convert-bag-to-deposit/blob/57e4ab9513d536c16121ad8916058d4102154138/src/test/resources/sample-jumpoff/3931-for-dataset-34359.html#L168-L169
      // looking for links with containing eiter of
      //   doi.org.*dans
      //   urn:nbn:nl:ui:13-
      filtered = hrefs.filter(_.matches("(?s).*(doi.org.*dans|urn:nbn:nl:ui:13-).*"))
      ids <- filtered.traverse(toDatasetId)
    } yield ids.filter(_.isDefined).map(_.get)
  }

  private def jumpoff(datasetId: String, fedoraProvider: FedoraProvider): Try[Option[String]] = {
    for {
      ids <- fedoraProvider.getSubordinates(datasetId)
      jumpofId = ids.find(_.startsWith("dans-jumpoff:"))
    } yield jumpofId
  }

  private def toDatasetId(str: String) = {
    val trimmed = str
      .replaceAll(".*doi.org/", "")
      .replaceAll(".*identifier=", "")
      .trim
    resolver.getDatasetId(trimmed)
  }
}
