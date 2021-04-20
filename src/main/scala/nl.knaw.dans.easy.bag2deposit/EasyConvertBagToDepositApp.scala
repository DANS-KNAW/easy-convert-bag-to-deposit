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
import nl.knaw.dans.easy.bag2deposit.ddm.Provenance
import nl.knaw.dans.easy.bag2deposit.ddm.Provenance.compare
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import java.io.{ FileNotFoundException, IOException }
import java.nio.charset.Charset
import scala.collection.mutable.ListBuffer
import scala.util.{ Failure, Success, Try }
import scala.xml.NodeSeq

class EasyConvertBagToDepositApp(configuration: Configuration) extends DebugEnhancedLogging {

  def addPropsToBags(bagParentDirs: Iterator[File],
                     maybeOutputDir: Option[File],
                     properties: DepositPropertiesFactory,
                    ): Try[FeedBackMessage] = {
    val triedString = bagParentDirs
      .map(addProps(properties, maybeOutputDir))
      .collectFirst { case Failure(e) => Failure(e) }
      .getOrElse(Success(s"No fatal errors")) // TODO show number of false/true values
    logMatchedReports()
    triedString
  }

  private val reportMatches = configuration.ddmTransformer.reportRewriteRule.reportMap.map(reportCfg =>
    reportCfg.uuid -> new ListBuffer[String]()
  ).toMap

  def registerMatchedReports(urn: String, reports: NodeSeq): Unit = {
    reports.foreach { node =>
      val reportUuid = (node \@ "valueURI").replaceAll(".*/", "")
      Try(reportMatches(reportUuid) += s"\t$urn\t${ node.text }")
        .getOrElse(logger.error(s"Could not register matched report $urn $reportUuid ${ node.text }"))
    }
  }

  def logMatchedReports(): Unit = {
    val uuidToReportLabel = configuration.ddmTransformer.reportRewriteRule.reportMap
      .map(r => r.uuid -> r.label).toMap
    reportMatches.foreach { case (reportUuid, foundReports) =>
      val reports = foundReports.toList
      if (reports.nonEmpty) {
        val label = uuidToReportLabel.getOrElse(reportUuid, reportUuid)
        logger.info(s"$label\n${ reports.mkString("\n") }")
      }
    }
  }

  private val provenance = new Provenance(
    app = getClass.getSimpleName,
    version = configuration.version
  )
  implicit val charset: Charset = Charset.forName("UTF-8")

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
      _ = logger.info(s"$bagInfo")
      metadata = bagDir / "metadata"
      ddmOld <- loadXml(metadata / "dataset.xml")
      props <- depositPropertiesFactory.create(bagInfo, ddmOld)
      datasetId = props.getString("identifier.fedora", "")
      ddmNew <- configuration.ddmTransformer.transform(ddmOld, datasetId)
      agreementsChanges <- configuration.agreementTransformer
        .transform(metadata / "depositor-info" / "agreements.xml")
      _ = provenance.xml(agreementsChanges, compare(ddmOld, ddmNew))
        .foreach(xml => (metadata / "provenance.xml").writeText(xml.serialize))
      _ = registerMatchedReports(datasetId, ddmNew \\ "reportNumber")
      _ = props.save((bagParentDir / "deposit.properties").toJava)
      _ = (metadata / "dataset.xml").writeText(ddmNew.serialize)
      _ = bagInfoKeysToRemove.foreach(mutableBagMetadata.remove)
      _ <- BagFacade.updateMetadata(bag)
      _ <- BagFacade.updateManifest(bag)
      _ = maybeOutputDir.foreach(move(bagParentDir))
      _ = logger.info(s"OK $datasetId ${ bagParentDir.name }/${ bagDir.name }")
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
