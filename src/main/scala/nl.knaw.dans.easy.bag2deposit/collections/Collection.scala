/*
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
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import nl.knaw.dans.easy.bag2deposit.FoXml.getEmd
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.csv.CSVFormat.RFC4180
import org.apache.commons.csv.{ CSVFormat, CSVParser, CSVRecord }
import resource.managed
import java.nio.charset.{ Charset, StandardCharsets }
import scala.collection.JavaConverters._
import scala.util.{ Failure, Try }
import scala.xml.{ Elem, Node, Text }

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

  private val skosCsvFormat = RFC4180
    .withHeader("URI", "prefLabel", "definition", "broader", "notation")
    .withDelimiter(',')
    .withRecordSeparator('\n')
    .withAutoFlush(true)

  private def parseSkosRecord(r: CSVRecord) = {
    r.get("prefLabel") ->
        <ddm:inCollection
           schemeURI="https://vocabularies.dans.knaw.nl/collections"
           valueURI={ r.get("URI") }
           subjectScheme="DANS Collection"
        >{ r.get("prefLabel") }</ddm:inCollection>
  }

   def getSeriesNode(collectionDatasetIds: Seq[String], maybeFedoraProvider: Option[FedoraProvider], seriesSet: Set[String]): Node = {
    val firstCollectionDatasetId = collectionDatasetIds.head
    if (seriesSet.contains(firstCollectionDatasetId)) {
      val parentEmd = getCollectionEmdXml(firstCollectionDatasetId, maybeFedoraProvider)
      val emdDescription = parentEmd.get \\ "description"
      <ddm:description descriptionType="SeriesInformation">{ emdDescription.head.text  }</ddm:description>
    }
    else
          Text("")
  }

  private def getCollectionEmdXml(datasetId: String, maybeFedoraProvider: Option[FedoraProvider]): Try[Node] = {
    maybeFedoraProvider.map { provider =>
      provider.loadFoXml(datasetId).flatMap(getEmd)
    }.getOrElse(Failure(new IllegalStateException(s"no EMD for $datasetId and no fedora configured")))
  }

  private def readSeriesFile(seriesFile: File): Set[String] = {
    if (seriesFile.exists)
      seriesFile.contentAsString.split("\n").toSet
    else
      Set()
  }

  /** @return collection-member-dataset-id -> <ddm:inCollection> */
  def getCollectionsMap(cfgDir: File, maybeFedoraProvider: Option[FedoraProvider]): Map[String, Seq[Node]] = {
    val skosFile = cfgDir / "excel2skos-collecties.csv"
    val collectionsFile = cfgDir / "ThemathischeCollecties.csv"
    val seriesFile = cfgDir / "seriesDatasetIds.txt"

    trace(skosFile, collectionsFile, seriesFile)
    val tuples = {
      for {
        skosRecords <- parseCsv(skosFile, skosCsvFormat)
        collectionRecords <- parseCsv(collectionsFile, collectionCsvFormat)
        originalCollections = collectionRecords.toList.map(parseCollectionRecord)
        skosMap = skosRecords.map(parseSkosRecord).toMap
        seriesSet = readSeriesFile(seriesFile)
      } yield originalCollections.flatMap { collection =>
        memberToCollections(skosMap, collection, maybeFedoraProvider, seriesSet )
      }
    }.doIfFailure { case e => logger.error(s"could not build CollectionsMap: $cfgDir $e", e) }
      .getOrElse(List.empty)
    tuples.groupBy(_._1).mapValues(_.flatMap(_._2))
  }

  private def memberToCollections(skosMap: Map[String, Elem], collection: Collection, maybeFedoraProvider: Option[FedoraProvider], seriesSet: Set[String]): Seq[(String, List[Node])] = {
    val name = collection.name
    lazy val default = <notImplemented>
      {s"$name not found in collections skos"}
    </notImplemented>
    val elem = skosMap.getOrElse(name, default)
    if (elem.toString().contains("not found"))
      logger.error(s"$name not found in collections skos")
    collection.members.map(id => id -> (elem +: getSeriesNode(collection.ids, maybeFedoraProvider, seriesSet)).toList)
  }

}
