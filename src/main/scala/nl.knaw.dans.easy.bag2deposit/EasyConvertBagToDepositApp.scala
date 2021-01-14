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

import better.files.File
import better.files.File.CopyOptions
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.bag2deposit.Command.FeedBackMessage
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import java.io.{ FileNotFoundException, IOException }
import scala.util.{ Failure, Success, Try }
import scala.xml.Node

class EasyConvertBagToDepositApp(configuration: Configuration) extends DebugEnhancedLogging {

  def addPropsToBags(bagParentDirs: Iterator[File],
                     maybeOutputDir: Option[File],
                     properties: DepositPropertiesFactory,
                    ): Try[FeedBackMessage] = {
    bagParentDirs
      .map(addProps(properties, maybeOutputDir))
      .collectFirst { case Failure(e) => Failure(e) }
      .getOrElse(Success(s"No fatal errors")) // TODO show number of false/true values
  }

  def formatDiff(generated: Node, modified: Node): String = {
    val original = normalized(generated).split("\n")
    val changed = normalized(modified).split("\n")
    original.diff(changed).mkString("===== some generated DDM\n\n", "\n", "\n\n") +
      changed.diff(original).mkString(s"===== is changed with ${ getClass.getSimpleName } ${ configuration.version } into\n\n", "\n", "\n")
  }

  private def addProps(depositPropertiesFactory: DepositPropertiesFactory, maybeOutputDir: Option[File])
                      (bagParentDir: File): Try[Boolean] = {
    logger.debug(s"creating application.properties for $bagParentDir")
    val bagInfoKeysToRemove = Seq(
      DansV0Bag.EASY_USER_ACCOUNT_KEY,
      BagInfo.baseUrnKey,
      BagInfo.baseDoiKey,
    )
    for {
      bagDir <- getBagDir(bagParentDir)
      bag <- BagFacade.getBag(bagDir)
      mutableBagMetadata = bag.getMetadata
      bagInfo <- BagInfo(bagDir, mutableBagMetadata)
      _ = logger.debug(s"$bagInfo")
      ddmFile = bagDir / "metadata" / "dataset.xml"
      ddmIn <- loadXml(ddmFile)
      ddmOut = configuration.ddmTransformer.transform(ddmIn).headOption
        .getOrElse(throw InvalidBagException("DDM transformation returned empty sequence"))
      _ = if (ddmIn != ddmOut) logger.info(formatDiff(ddmIn, ddmOut))
      _ = ddmFile.writeText(ddmOut.serialize)
      props <- depositPropertiesFactory.create(bagInfo, ddmOut)
      _ = props.save((bagParentDir / "deposit.properties").toJava)
      _ = bagInfoKeysToRemove.foreach(mutableBagMetadata.remove)
      _ <- BagFacade.updateMetadata(bag)
      _ <- BagFacade.updateManifest(bag)
      _ = maybeOutputDir.foreach(move(bagParentDir))
      _ = logger.info(s"OK $bagParentDir")
    } yield true
  }.recoverWith {
    case e: InvalidBagException =>
      logger.error(s"${ bagParentDir.name } failed: ${ e.getMessage }")
      Success(false)
    case e: FileNotFoundException =>
      logger.error(s"${ bagParentDir.name } failed: ${ e.getMessage }")
      Success(false)
    case e: Throwable =>
      logger.error(s"${ bagParentDir.name } failed with not expected error: ${ e.getClass.getSimpleName } ${ e.getMessage }")
      Failure(e)
  }

  private def move(bagParentDir: File)(outputDir: File) = {
    val target = outputDir / bagParentDir.name
    logger.info(s"moving bag-parent from $bagParentDir to $target")
    bagParentDir.moveTo(target)(CopyOptions.atomically)
  }

  private def getBagDir(bagParentDir: File): Try[File] = Try {
    val children = bagParentDir.children.toList
    if (children.size > 1)
      throw InvalidBagException(s"more than just one item in $bagParentDir")
    children.find(_.isDirectory).getOrElse(
      throw InvalidBagException(s"could not find a directory in the deposit $bagParentDir")
    )
  }.recoverWith {
    case e: IOException =>
      // for example: java.nio.file.NotDirectoryException: /path/to/UUID/deposit.properties
      Failure(InvalidBagException(s"could not look up a bag in the deposit: $e"))
  }
}
