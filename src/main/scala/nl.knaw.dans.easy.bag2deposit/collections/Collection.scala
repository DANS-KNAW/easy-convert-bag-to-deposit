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

import better.files.{ Dispose, File }
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
import org.apache.commons.csv.{ CSVFormat, CSVParser, CSVPrinter, CSVRecord }
import org.joda.time.DateTime.now
import resource.managed

import java.nio.charset.{ Charset, StandardCharsets }
import scala.collection.JavaConverters._
import scala.util.{ Failure, Try }
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

  private val collectionCsvFormat = RFC4180
    .withHeader("name", "EASY-dataset-id", "type", "comment", "members")
    .withDelimiter(',')
    .withRecordSeparator('\n')
    .withAutoFlush(true)

  private def parseCollectionRecord(r: CSVRecord) = Try {
    val memberIds = Try(r.get("members"))
      .map(_.split(", *"))
      .getOrElse(Array[String]())
    Collection(
      r.get("name"),
      r.get("EASY-dataset-id").split(", *").filter(_.startsWith("easy-dataset:")),
      Try(r.get("type")).getOrElse(""), // TODO assuming IllegalArgumentException
      Try(r.get("comment")).getOrElse(""),
      memberIds, // compiler error when inlined
    )
  }.getOrElse(throw new IllegalArgumentException(s"invalid line $r"))

  private def writeCollectionRecord(printer: CSVPrinter, c: Collection): Try[Collection] = Try {
    printer.printRecord(c.name, c.ids.mkString(","), c.collectionType, c.comment, c.members.mkString(","))
    c
  }

  private val skosCsvFormat = RFC4180
    .withHeader("URI", "prefLabel", "definition", "broader", "notation")
    .withDelimiter(',')
    .withRecordSeparator('\n')
    .withAutoFlush(true)

  private def parseSkosRecord(r: CSVRecord) = {
    r.get("definition") ->
        <ddm:inCollection
           schemeURI="http://easy.dans.knaw.nl/vocabularies/collecties"
           valueURI={ r.get("URI") }
           subjectScheme="DANS Collection"
        >{ r.get("prefLabel") }</ddm:inCollection>
  }

  /** @return collection-member-dataset-id -> <ddm:inCollection> */
  def getCollectionsMap(cfgDir: File)(fedoraProvider: FedoraProvider): Map[String, Elem] = {
    val skosFile = cfgDir / "excel2skos-collecties.csv"
    val collectionsFile = cfgDir / "ThemathischeCollecties.csv"

    def updateCollections(originalCollections: Seq[Collection]): Try[List[Collection]] = {
      def updateWhenNotProvided(original: Collection)(implicit printer: CSVPrinter): Try[Collection] = {
        trace(original)
        val updated = if (original.members.nonEmpty) original
                      else original.copy(members = original.ids.flatMap(membersOf(fedoraProvider)))
        writeCollectionRecord(printer, updated).map(_ => updated)
      }

      for {
        _ <- Try(collectionsFile.moveTo(File(collectionsFile.toString().replace(".csv", s"-$now.csv"))))
        updates <- new Dispose(collectionCsvFormat.print(collectionsFile.newFileWriter()))
          .apply(implicit csvPrinter =>
            originalCollections.map(updateWhenNotProvided).toList.sequence
          )
      } yield updates
    }

    trace(skosFile, collectionsFile)
    for {
      skosRecords <- parseCsv(skosFile, skosCsvFormat)
      collectionRecords <- parseCsv(collectionsFile, collectionCsvFormat)
      originalCollections = collectionRecords.toList.map(parseCollectionRecord)
      skosMap = skosRecords.map(parseSkosRecord).toMap
      updatedCollections <- updateCollections(originalCollections)
    } yield updatedCollections.flatMap { collection =>
      val name = collection.name
      lazy val default = <notImplemented>{ s"$name not found in collections skos" }</notImplemented>
      val elem = skosMap.getOrElse(name, default)
      collection.members.map(id => id -> elem)
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
      maybeJumpoffId <- fedoraProvider.getJumpoff(datasetId)
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
