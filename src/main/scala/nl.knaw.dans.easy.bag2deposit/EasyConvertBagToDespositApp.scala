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
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.bag2deposit.BagSource.VAULT
import nl.knaw.dans.easy.bag2deposit.Command.FeedBackMessage
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Failure, Success, Try }
import scala.xml.XML

class EasyConvertBagToDespositApp(configuration: Configuration) extends DebugEnhancedLogging {

  def addPropsToBags(bagParentDirs: Iterator[File],
                     maybeOutputDir: Option[File],
                     properties: DepositPropertiesFactory,
                    ): Try[FeedBackMessage] = {
    bagParentDirs
      .map(addProps(properties, maybeOutputDir))
      .collectFirst { case Failure(e) => Failure(e) }
      .getOrElse(Success(s"See logging")) // TODO show number of false/true values
  }

  private def addProps(factory: DepositPropertiesFactory, maybeOutputDir: Option[File])
                      (bagParentDir: File): Try[Boolean] = {
    logger.debug(s"creating application.properties for $bagParentDir")
    val requireBaseUrnWithVersionOf = factory.bagSource == VAULT // TODO less sneaky
    val bagInfoKeysToRemove = Seq(
      DansV0Bag.EASY_USER_ACCOUNT_KEY,
      BagInfo.baseUrnKey,
    )
    for {
      bagDir <- getBagDir(bagParentDir)
      bag <- BagFacade.getBag(bagDir)
      mutableBagMetadata = bag.getMetadata
      bagInfo <- BagInfo(bagDir, mutableBagMetadata, requireBaseUrnWithVersionOf)
      _ = logger.debug(s"$bagInfo")
      ddm = XML.loadFile((bagDir / "metadata" / "dataset.xml").toJava)
      props <- factory.create(bagInfo, ddm)
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
