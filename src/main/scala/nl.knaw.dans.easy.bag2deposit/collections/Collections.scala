package nl.knaw.dans.easy.bag2deposit.collections

import better.files.File
import cats.instances.list._
import cats.instances.try_._
import cats.syntax.traverse._
import nl.knaw.dans.lib.error.TryExtensions
import org.apache.commons.csv.CSVFormat.RFC4180
import org.apache.commons.csv.{ CSVFormat, CSVParser, CSVRecord }
import resource.managed

import java.nio.charset.Charset.defaultCharset
import scala.collection.JavaConverters.iterableAsScalaIterableConverter
import scala.util.Try
import scala.xml.{ Elem, XML }

object Collections {
  val resolver: Resolver = Resolver()

  private def parseCsv(file: File, format: CSVFormat): Try[Iterable[CSVRecord]] = {
    managed(CSVParser.parse(
      file.toJava,
      defaultCharset(),
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

  def memberDatasetIdToInCollection(collectionDatasetIdToInCollection: Seq[(String, Elem)], fedoraProvider: FedoraProvider): Seq[(String, Elem)] = {

    collectionDatasetIdToInCollection
      .flatMap { case (datasetId, inCollection) =>
        membersOf(datasetId, fedoraProvider)
          .unsafeGetOrThrow
          .map(_ -> inCollection)
      }
  }

  def membersOf(datasetId: String, fedoraProvider: FedoraProvider): Try[Seq[String]] = {
    for {
      maybeJumpoffId <- jumpoff(datasetId, fedoraProvider)
      jumpoffId = maybeJumpoffId.getOrElse(throw new Exception(s"no jumpoff for $datasetId"))
      html <- fedoraProvider
        .disseminateDatastream(jumpoffId, "HTML_MU")
        .map(XML.load).tried
      ids <- members(html)
    } yield ids
  }

  private def jumpoff(datasetId: String, fedoraProvider: FedoraProvider): Try[Option[String]] = {
    for {
      ids <- fedoraProvider.getSubordinates(datasetId)
      jumpofId = ids.find(_.startsWith("dans-jumpoff:"))
    } yield jumpofId
  }

  /**
   *
   * @param xhtml content of jump off stream, for example
   *              http://easy.dans.knaw.nl:8080/fedora/objects/dans-jumpoff:3931/datastreams/HTML_MU/content
   * @return links to doi/urn-s resolved to dataset IDs
   */
  def members(xhtml: Elem): Try[List[String]] = (xhtml \\ "a")
    .map(_ \@ "href")
    .filter(_.matches(".*(doi.org.*dans|urn:nbn:nl:ui:13-).*"))
    .toList
    // duplicates like // urn:nbn:nl:ui:13-6wni-xz in easy-dataset:34359
    // and lots in easy-dataset:6460 by name, by interview nr and hidden ones with [ZWNBSP]
    .sortBy(identity).distinct
    .traverse(id => toDatasetId(id))

  private def toDatasetId(str: String) = {
    val trimmed = str
      .replaceAll(".*doi.org/", "")
      .replaceAll(".*identifier=", "")
      .trim
    resolver.getDatasetId(trimmed)
  }
}
