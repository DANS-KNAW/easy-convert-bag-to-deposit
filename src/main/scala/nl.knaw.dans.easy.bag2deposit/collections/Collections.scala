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
import com.yourmediashelf.fedora.client.FedoraClientException
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.csv.CSVFormat.RFC4180
import org.apache.commons.csv.{ CSVFormat, CSVParser, CSVRecord }
import resource.managed

import java.nio.charset.{ Charset, StandardCharsets }
import scala.collection.JavaConverters._
import scala.util.{ Failure, Try }
import scala.xml.Elem

object Collections extends DebugEnhancedLogging {

  private val browser = JsoupBrowser()
  private val resolver: Resolver = Resolver()

  private def parseCsv(file: File, format: CSVFormat): Try[Iterable[CSVRecord]] = {
    trace(file)
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
    trace(())
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

    logger.info(s"building collections from $cfgDir")

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

  def memberDatasetIdToInCollection(collectionDatasetIdToInCollection: Seq[(String, Elem)], fedoraProvider: FedoraProvider): Map[String, Elem] = Try {
    collectionDatasetIdToInCollection
      .flatMap { case (datasetId, inCollection) =>
        logger.info(s"looking for members of $datasetId for ${ inCollection.text }")
        membersOf(datasetId, fedoraProvider)
          .map(_ -> inCollection)
      }.toMap
  }.doIfFailure { case e => logger.error(s"could not build collectionMap: $e", e) }
    .getOrElse(Map.empty)

  private def membersOf(datasetId: String, fedoraProvider: FedoraProvider): Seq[String] = {
    def getMu(jumpoffId: String, streamId: String) = {
      fedoraProvider
        .disseminateDatastream(jumpoffId, streamId)
        .map(browser.parseInputStream(_, StandardCharsets.UTF_8.name()))
        .tried
    }

    def getMuAsHtmlDoc(jumpoffId: String) = {
      getMu(jumpoffId, "HTML_MU")
        .recoverWith {
          case e: FedoraClientException if e.getStatus == 404 =>
            logger.warn(s"no HTML_MU for $jumpoffId, trying TXT_MU")
            getMu(jumpoffId, "TXT_MU")
          case e =>
            trace(e)
            Failure(e)
        }
    }

    // (?s) matches multiline values like https://github.com/DANS-KNAW/easy-convert-bag-to-deposit/blob/57e4ab9513d536c16121ad8916058d4102154138/src/test/resources/sample-jumpoff/3931-for-dataset-34359.html#L168-L169
    // looking for links containing eiter of
    //   doi.org.*dans
    //   urn:nbn:nl:ui:13-
    val regexp = "(?s).*(doi.org.*dans|urn:nbn:nl:ui:13-).*"
    for {
      maybeJumpoffId <- jumpoff(datasetId, fedoraProvider)
      jumpoffId = maybeJumpoffId.getOrElse(throw new Exception(s"no jumpoff for $datasetId"))
      doc <- getMuAsHtmlDoc(jumpoffId)
      items = doc >> elementList("a")
      hrefs = items
        .withFilter(_.hasAttr("href"))
        .map(_.attr("href"))
        .sortBy(identity)
        .distinct
      maybeIds = hrefs.withFilter(_.matches(regexp)).map(toDatasetId)
    } yield maybeIds.withFilter(_.isDefined).map(_.get)
  }.doIfFailure { case e => logger.error(s"could not find members of $datasetId: $e", e) }
    .getOrElse(Seq.empty)

  private def jumpoff(datasetId: String, fedoraProvider: FedoraProvider): Try[Option[String]] = {
    for {
      ids <- fedoraProvider.getSubordinates(datasetId)
      jumpofId = ids.find(_.startsWith("dans-jumpoff:"))
    } yield jumpofId
  }

  private def toDatasetId(str: String): Option[String] = {
    val trimmed = str
      .replaceAll(".*doi.org/", "")
      .replaceAll(".*identifier=", "")
      .trim
    resolver.getDatasetId(trimmed)
  }.doIfFailure { case e =>
    logger.error(s"could not resolve $str: $e", e)
  }.getOrElse {
    logger.warn(s"resolver could not find $str")
    None
  }
}
