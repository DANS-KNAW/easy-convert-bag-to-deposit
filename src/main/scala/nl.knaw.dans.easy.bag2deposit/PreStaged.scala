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
package nl.knaw.dans.easy.bag2deposit

import better.files.{ Dispose, File }
import org.apache.commons.csv.CSVFormat.RFC4180
import org.json4s.native.JsonMethods.parse
import org.json4s.{ DefaultFormats, Formats }
import scalaj.http.{ Http, HttpResponse }

import java.net.URI
import java.nio.file.{ Path, Paths }
import scala.util.{ Failure, Try }

/**
 * @param path      as in the manifest file
 * @param storageId as returned from the migration-info-service
 */
case class PreStaged(path: Path,
                     fileSize: Double,
                     mimeType: String,
                     checksumType: String,
                     checksumValue: String,
                     storageId: String,
                    )
object PreStaged {
  private val csvFormat = RFC4180
    .withHeader("path", "checksum", "storageId")
    .withDelimiter(',')
    .withRecordSeparator('\n')
    .withAutoFlush(true)

  def write(seq: Seq[PreStaged], metadataDir: File): Try[Unit] = Try {
    trace(seq)
    new Dispose(csvFormat.print((metadataDir / "pre-staged.csv").newFileWriter()))
      .apply(implicit csvPrinter =>
        seq.foreach(r =>
          csvPrinter.printRecord(r.path, r.checksumValue, r.storageId)
        )
      )
  }

  private case class CheckSum(`@type`: String, `@value`: String)
  private case class DataFile(storageIdentifier: String, fileName: String, mimeType: String, checksum: CheckSum, fileSize: Double)
  private case class MigrationInfo(label: String, directoryLabel: String, datasetSequenceNumber: String, dataFile: DataFile)
  private implicit val jsonFormats: Formats = new DefaultFormats {}

  def apply(json: String): Try[List[PreStaged]] = Try{
    parse(json, useBigDecimalForDouble = true)
      .extract[List[MigrationInfo]]
      .map(mi => new PreStaged(
        Paths.get(s"${ mi.directoryLabel }/${ mi.dataFile.fileName }"),
        mi.dataFile.fileSize,
        mi.dataFile.mimeType,
        mi.dataFile.checksum.`@type`,
        mi.dataFile.checksum.`@value`,
        mi.dataFile.storageIdentifier,
      ))
  }
}
case class PreStagedProvider(migrationInfoUri: URI) {
  def get(datasetId: String, seqNr: Int = 1): Try[Seq[PreStaged]] = {
    find(s"/datasets/$datasetId/seq/$seqNr/basic-file-metas")
      .flatMap(PreStaged(_))
  }

  private def find(q: String): Try[String] = Try {
    execute(q)
  }.recoverWith {
    case t: Throwable => Failure(new Exception(s"$q ${ t.getMessage }", t))
  }.map {
    case response if response.code == 404 => "[]" // an empty list
    case response if response.code == 200 => response.body
    case response => throw new Exception(
      s"Not expected response code from dd-migration-info. $q, response: ${ response.code } - ${ response.body }",
      null,
    )
  }

  def execute(q: String): HttpResponse[String] = {
    trace(migrationInfoUri, q)
    Http(migrationInfoUri.resolve(q).toString)
      .header("Accept", "text/json")
      .asString
  }
}
