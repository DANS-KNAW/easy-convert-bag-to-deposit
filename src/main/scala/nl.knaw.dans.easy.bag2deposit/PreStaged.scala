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
package nl.knaw.dans.easy.bag2deposit

import better.files.{ Dispose, File }
import org.apache.commons.csv.CSVFormat.RFC4180
import org.json4s.native.JsonMethods.parse
import org.json4s.{ DefaultFormats, Formats }
import scalaj.http.{ Http, HttpResponse }

import java.io.IOException
import java.net.URI
import java.nio.file.{ Path, Paths }
import scala.util.{ Failure, Success, Try }

/**
 * @param path      as in the manifest file
 * @param storageId as returned from the migration-info-service
 */
case class PreStaged(path: Path,
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
  private case class PrestagedFile(storageIdentifier: String, fileName: String, mimeType: String, checksum: CheckSum)
  private case class MigrationInfo(label: String, directoryLabel: Option[String], versionSequenceNumber: String, prestagedFile: PrestagedFile)
  private implicit val jsonFormats: Formats = new DefaultFormats {}

  def apply(json: String): Try[List[PreStaged]] = Try {
    val migrationInfoes = parse(json, useBigDecimalForDouble = true)
      .extract[List[MigrationInfo]]
    val preStagedFiles = migrationInfoes
      .map(mi => new PreStaged(
        Paths.get(s"${ mi.directoryLabel.map(l => s"$l/").getOrElse("") }${ mi.prestagedFile.fileName }"),
        mi.prestagedFile.mimeType,
        mi.prestagedFile.checksum.`@type`,
        mi.prestagedFile.checksum.`@value`,
        mi.prestagedFile.storageIdentifier,
      ))
    preStagedFiles.groupBy(_.checksumValue)
      .filter(_._2.size == 1)
      .values.flatten.toList
  }
}
case class PreStagedProvider(migrationInfoUri: URI) {
  def get(doi: String, seqNr: Int = 1): Try[Seq[PreStaged]] = {
    find(s"/datasets/:persistentId/seq/$seqNr/basic-file-metas?persistentId=doi:$doi")
      .flatMap(PreStaged(_))
  }

  private def find(q: String): Try[String] = Try {
    execute(q)
  }.recoverWith {
    case t: Throwable => Failure(new IOException(s"dd-migration-info failure with $q CAUSE: $t", t))
  }.map {
    case response if response.code == 404 => "[]" // an empty list
    case response if response.code == 200 => response.body
    case response => throw new IOException(
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
