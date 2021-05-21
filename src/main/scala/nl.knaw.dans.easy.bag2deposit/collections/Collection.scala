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
import cats.implicits.catsStdInstancesForTry
import cats.instances.list._
import cats.syntax.traverse._
import com.yourmediashelf.fedora.client.FedoraClientException
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.csv.CSVFormat.RFC4180
import org.apache.commons.csv.{ CSVFormat, CSVParser, CSVRecord }
import org.joda.time.DateTime.now
import resource.managed

import java.io.FileWriter
import java.nio.charset.{ Charset, StandardCharsets }
import scala.collection.JavaConverters._
import scala.util.{ Failure, Success, Try }
import scala.xml.Elem

case class Collection(name: String, ids: Seq[String], collectionType: String, comment: String, members: Seq[String])

object Collection extends DebugEnhancedLogging {

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

  private val collectionCsvFormat = RFC4180.withHeader("name", "ids", "type", "comment", "members")

  private def writeCollectionRecord(writer: FileWriter, c: Collection): Try[Collection] = Try {
    collectionCsvFormat.printRecord(writer, c.name, c.ids.mkString(","), c.collectionType, c.comment, c.members.mkString(","))
    c
  }

  private def parseCollectionRecord(r: CSVRecord) = Try {
    val memberIds = Try(r.get("members"))
      .map(_.split(", *"))
      .getOrElse(Array[String]())
    Collection(
      r.get("name"),
      r.get("ids").split(", *").filter(_.startsWith("easy-dataset:")),
      Try(r.get("type")).getOrElse(""), // TODO assuming IllegalArgumentException
      Try(r.get("comment")).getOrElse(""),
      memberIds, // compiler error when inlined
    )
  }.getOrElse(throw new IllegalArgumentException(s"invalid line $r"))

  private val skosCsvFormat = RFC4180.withHeader("URI", "prefLabel", "definition", "broader", "notation")

  private def parseSkosRecord(r: CSVRecord) = {
    r.get("definition") ->
        <ddm:inCollection
           schemeURI="http://easy.dans.knaw.nl/vocabularies/collecties"
           valueURI={ r.get("URI") }
           subjectScheme="DANS Collection"
        >{ r.get("prefLabel") }</ddm:inCollection>
  }

  /** @return collection-member-dataset-id -> <ddm:inCollection> */
  def getCollectionsMap(cfgDir: File, maybeFedoraProvider: Option[FedoraProvider]): Map[String, Elem] = {
    val skosFile = cfgDir / "excel2skos-collecties.csv"
    val collectionsFile = cfgDir / "ThemathischeCollecties.csv"

    def updateCollections(originalCollections: Seq[Collection], fedoraProvider: FedoraProvider): Try[List[Collection]] = {
      for { // TODO unit test writes to src/main/assembly/dist/cfg
        _ <- Try(collectionsFile.moveTo(File(collectionsFile.toString().replace(".csv", s"-$now.csv"))))
        writer = collectionsFile.newFileWriter()
        updated <- originalCollections.toList.map { original =>
          trace(original)
          writeCollectionRecord(
            writer,
            if (original.members.nonEmpty) original
            else original.copy(members = original.ids.flatMap(membersOf(fedoraProvider)))
          )
        }.sequence
      } yield updated
    }

    trace(skosFile, collectionsFile)
    for {
      skosRecords <- parseCsv(skosFile, skosCsvFormat)
      collectionRecords <- parseCsv(collectionsFile, collectionCsvFormat)
      originalCollections = collectionRecords.toList.map(parseCollectionRecord)
      skosMap = skosRecords.map(parseSkosRecord).toMap
      updatedCollections <- maybeFedoraProvider
        .map(fedora => updateCollections(originalCollections, fedora))
        .getOrElse(Success(originalCollections))
    } yield updatedCollections.flatMap { collection =>
      val name = collection.name
      lazy val default = <notImplemented>{ s"$name not found in collections skos" }</notImplemented>
      val elem = skosMap.getOrElse(name, default)
      collection.members.map(id => id -> elem).toMap
    }.toMap
  }.doIfFailure { case e => logger.error(s"could not build CollectionsMap: $cfgDir $e", e) }
    .getOrElse(Map.empty)

  private def membersOf(fedoraProvider: FedoraProvider)(datasetId: String): Seq[String] = {
    trace(datasetId)

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
