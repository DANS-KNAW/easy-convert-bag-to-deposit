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
import nl.knaw.dans.easy.bag2deposit.Command.FeedBackMessage
import nl.knaw.dans.easy.bag2deposit.FoXml.getAmd
import nl.knaw.dans.easy.bag2deposit.ddm.Provenance
import nl.knaw.dans.easy.bag2deposit.ddm.Provenance.compare
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.configuration.PropertiesConfiguration

import java.io.{ FileNotFoundException, IOException }
import java.nio.charset.Charset
import java.nio.file.Paths
import scala.collection.mutable.ListBuffer
import scala.util.{ Failure, Success, Try }
import scala.xml._

class EasyConvertBagToDepositApp(configuration: Configuration) extends DebugEnhancedLogging {

  private val printer = new PrettyPrinter(160, 2)

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
    trace(urn)
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
    val changedMetadata = Seq(
      "bag-info.txt",
      "metadata/amd.xml", 
      "metadata/emd.xml", 
      "metadata/files.xml",
      "metadata/dataset.xml",
      "metadata/provenance.xml",
    ).map(Paths.get(_))
    val bagInfoKeysToRemove = Seq(
      BagFacade.EASY_USER_ACCOUNT_KEY,
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
      migration = bagDir / "data" / "easy-migration"
      ddmFile = metadata / "dataset.xml"
      ddmIn <- loadXml(ddmFile)
      depositProps <- depositPropertiesFactory.create(bagInfo, ddmIn)
      fromVault = depositProps.getString("deposit.origin") == "VAULT"
      datasetId = depositProps.getString("identifier.fedora", "")
      ddmOut <- configuration.ddmTransformer.transform(ddmIn, datasetId)
      _ = registerMatchedReports(datasetId, ddmOut \\ "reportNumber")
      oldDcmi = (ddmIn \ "dcmiMetadata").headOption.getOrElse(<dcmiMetadata/>)
      newDcmi = (ddmOut \ "dcmiMetadata").headOption.getOrElse(<dcmiMetadata/>)
      amdFile = metadata / "amd.xml"
      amdIn <- getAmdXml(datasetId, amdFile)
      amdOut <- configuration.userTransformer.transform(amdIn)
      maybeProvenance = provenance.collectChangesInXmls(Map(
      "http://easy.dans.knaw.nl/easy/dataset-administrative-metadata/" -> compare(amdIn, amdOut),
      "http://easy.dans.knaw.nl/schemas/md/ddm/" -> compare(oldDcmi, newDcmi),
      ))
      _ = bagInfoKeysToRemove.foreach(mutableBagMetadata.remove)
      _ = depositProps.setProperty("depositor.userId", (amdOut \ "depositorId").text)
      _ = depositProps.save((bagParentDir / "deposit.properties").toJava) // N.B. the first write action
      _ = ddmFile.writeText(ddmOut.serialize)
      _ = amdFile.writeText(amdOut.serialize)
      _ = maybeProvenance.foreach(xml => (metadata / "provenance.xml").writeText(xml.serialize))
      _ = copyMigrationFiles(metadata, migration, fromVault)
      _ = trace("updating metadata")
      _ <- BagFacade.updateMetadata(bag)
      _ = trace("updating payload manifest")
      _ <- BagFacade.updatePayloadManifests(bag, Paths.get("data/easy-migration"))
      _ = trace("writing payload manifests")
      _ <- BagFacade.writePayloadManifests(bag)
      _ = trace("updating tag manifest")
      _ <- BagFacade.updateTagManifests(bag, changedMetadata)
      _ = trace("writing tagmanifests")
      _ <- BagFacade.writeTagManifests(bag)
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

  private def getAmdXml(datasetId: String, amdFile: File): Try[Node] = {
    if (amdFile.exists)
      loadXml(amdFile)
    else {
      configuration.fedoraProvider.map { provider =>
        provider.loadFoXml(datasetId).flatMap(getAmd)
      }.getOrElse(Failure(new IllegalStateException(s"no AMD for $datasetId and no fedora configured")))
    }
  }

  private def copyMigrationFiles(metadata: File, migration: File, fromVault: Boolean): Try[Unit] = Try {
    val filesXmlFile = (metadata / "files.xml").toString()
    val migrationFiles = Seq("provenance.xml", "dataset.xml", "files.xml", "emd.xml")
    val migrationDir = migration.createDirectories()
    migrationFiles.foreach(name => (metadata / name).copyTo(migrationDir / name))
    addToXmlFile(filesXmlFile, migrationFiles)
  }

  private def addToXmlFile(filesXmlFile: String, filesToAdd: Seq[String]): Try[File] = Try {
    val oldFilesXml = XML.loadFile(filesXmlFile)
    val newFilesXml = FilesXml(oldFilesXml, "data/easy-migration", filesToAdd, "text/xml")
    File(filesXmlFile).delete()
    // Notice: here we use PrettyPrinter to format the xml-file, to get all the new elements line up neatly.
    // However, generally we discourage its usage because PrettyPrinter removes white spaces and this may cause
    // problems, for instance in EMD.xml and DDM.xml.  Usage of 'serialize' is encouraged
    // (https://github.com/DANS-KNAW/easy-convert-bag-to-deposit/blob/23f0eb93dbc2cfa9cddd78904a0c5b9a1f63eede/src/main/scala/nl.knaw.dans.easy.bag2deposit/package.scala#L69-L72)
    File(filesXmlFile).writeText(printer.format(newFilesXml))
  }

  private def move(bagParentDir: File)(outputDir: File) = {
    trace(bagParentDir, outputDir)
    val target = outputDir / bagParentDir.name
    logger.info(s"moving bag-parent from $bagParentDir to $target")
    bagParentDir.moveTo(target)(CopyOptions.atomically)
  }

  private def getBagDir(bagParentDir: File): Try[File] = Try {
    trace(bagParentDir)
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
