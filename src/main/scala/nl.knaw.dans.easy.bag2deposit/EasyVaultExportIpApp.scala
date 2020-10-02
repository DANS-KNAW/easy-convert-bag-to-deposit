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

import java.io.{ FileNotFoundException, IOException }

import better.files.File
import better.files.File.CopyOptions
import nl.knaw.dans.easy.bag2deposit.Command.FeedBackMessage
import nl.knaw.dans.easy.bag2deposit.IdType.IdType
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Failure, Success, Try }
import scala.xml.XML

class EasyVaultExportIpApp(configuration: Configuration) extends DebugEnhancedLogging {

  def addPropsToBags(bagParentDirs: Iterator[File], idType: IdType, maybeOutputDir: Option[File], properties: DepositPropertiesFactory): Try[FeedBackMessage] = {
    bagParentDirs
      .map(addProps(properties, idType, maybeOutputDir))
      .collectFirst { case Failure(e) => Failure(e) }
      .getOrElse(Success(s"See logging")) // TODO show number of false/true values
  }

  private def addProps(factory: DepositPropertiesFactory, idType: IdType, maybeOutputDir: Option[File])
                      (bagParentDir: File): Try[Boolean] = {
    logger.debug(s"creating application.properties for $bagParentDir")
    for {
      metadataDir <- getMetadataDir(bagParentDir)
      bagInfo <- BagInfo(metadataDir / ".." / "bag-info.txt")
      _ = logger.debug(s"$bagInfo")
      ddm = XML.loadFile((metadataDir / "dataset.xml").toJava)
      props <- factory.create(bagInfo, ddm, idType)
      _ = props.save((bagParentDir / "deposit.properties").toJava)
      _ = maybeOutputDir.foreach(move(bagParentDir))
      _ = logger.info(s"OK $bagParentDir")
    } yield true
  }.recoverWith {
    case e: InvalidBagException =>
      logger.error(s"$bagParentDir failed: ${ e.getMessage }")
      Success(false)
    case e: FileNotFoundException =>
      logger.error(s"$bagParentDir failed: ${e.getMessage}")
      Success(false)
  }

  private def move(bagParentDir: File)(outputDir: File) = {
    val target = outputDir / bagParentDir.name
    logger.info(s"moving bag-parent from $bagParentDir to $target")
    bagParentDir.moveTo(target)(CopyOptions.atomically)
  }

  private def getMetadataDir(bagParentDir: File): Try[File] = {
    val triedDir = for {
      dirs <- Try { bagParentDir.children.flatMap(_.children.filter(dir => dir.isDirectory && dir.name == "metadata")).toList }
      _ = if (dirs.size > 1) throw InvalidBagException(s"more than one */metadata")
      dir = dirs.headOption.getOrElse(throw InvalidBagException(s"no */metadata"))
    } yield dir
    triedDir.recoverWith {
      case e: IOException =>
        // for example: java.nio.file.NotDirectoryException: /path/to/UUID/deposit.properties
        Failure(InvalidBagException(s"could not look up */metadata: $e"))
    }
  }
}
