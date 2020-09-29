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
package nl.knaw.dans.easy.v2ip

import java.io.FileNotFoundException
import java.util.UUID

import better.files.File
import better.files.File.CopyOptions
import nl.knaw.dans.easy.v2ip.Command.FeedBackMessage
import nl.knaw.dans.easy.v2ip.IdType.IdType
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Failure, Success, Try }
import scala.xml.XML

class EasyVaultExportIpApp(configuration: Configuration) extends DebugEnhancedLogging {

  def addPropsToSips(sipDirs: Iterator[File], idType: IdType, maybeOutputDir: Option[File], properties: DepositProperties): Try[FeedBackMessage] = {
    sipDirs
      .map(addProps(properties, idType, maybeOutputDir))
      .collectFirst { case Failure(e) => Failure(e) }
      .getOrElse(Success(s"See logging")) // TODO show number of false/true values
  }

  private def addProps(properties: DepositProperties, idType: IdType, maybeOutputDir: Option[File])
                      (sipDir: File): Try[Boolean] = {
    logger.debug(s"creating application.properties for $sipDir")
    for {
      metadataDir <- getMetadataDir(sipDir)
      bagInfo <- BagInfo(metadataDir / ".." / "bag-info.txt")
      _ = logger.debug(s"$bagInfo")
      ddm = XML.loadFile((metadataDir / "dataset.xml").toJava)
      props <- properties.fill(bagInfo, ddm)
      _ = props.save((sipDir / "deposit.properties").toJava)
      _ = maybeOutputDir.foreach(move(sipDir))
      _ = logger.info(s"OK $sipDir")
    } yield true
  }.recoverWith {
    case e: InvalidBagException =>
      logger.error(s"$sipDir failed: ${ e.getMessage }")
      Success(false)
    case e: FileNotFoundException =>
      logger.error(s"$sipDir failed: file not found ${ e.getMessage }")
      Success(false)
  }

  private def move(sipDir: File)(outputDir: File) = {
    val target = outputDir / sipDir.name
    logger.info(s"moving SIP from $sipDir to $target")
    sipDir.moveTo(target)(CopyOptions.atomically)
  }

  private def getMetadataDir(sipDir: File): Try[File] = {
    def fail(prefix: String) = Failure(InvalidBagException(
      s"$prefix */metadata directory found in ${ sipDir.toJava.getAbsolutePath }")
    )

    val dirs = sipDir.children.flatMap(_.children.filter(dir => dir.isDirectory && dir.name == "metadata")).toList
    if (dirs.size > 1) fail("more than one")
    else dirs.map(Success(_)).headOption.getOrElse(fail("no"))
  }
}
