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

import better.files.File
import better.files.File.CopyOptions
import nl.knaw.dans.easy.bag2deposit.Command.FeedBackMessage
import nl.knaw.dans.easy.bag2deposit.FoXml.getAmd
import nl.knaw.dans.easy.bag2deposit.ddm.Provenance
import nl.knaw.dans.easy.bag2deposit.ddm.Provenance.{ compareAMD, compareDDM }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import java.io.{ FileNotFoundException, IOException }
import java.nio.charset.Charset
import java.nio.file.Paths
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
    triedString
  }

  private val provenance = new Provenance(app = getClass.getSimpleName, version = configuration.version)
  implicit val charset: Charset = Charset.forName("UTF-8")

  private def addProps(depositPropertiesFactory: DepositPropertiesFactory, maybeOutputDir: Option[File])
                      (bagParentDir: File): Try[Boolean] = {
    logger.debug(s"creating application.properties for $bagParentDir")
    val bagInfoKeysToRemove = Seq(
      BagFacade.EASY_USER_ACCOUNT_KEY,
      BagFacade.BAG_SEQUENCE_NUMBER,
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
      (ddmIn, oldDdmChars, newDdmChars) <- loadXml(ddmFile)
      depositProps <- depositPropertiesFactory.create(bagInfo, ddmIn)
      fromVault = depositProps.getString("deposit.origin") == "VAULT"
      datasetId = depositProps.getString("identifier.fedora", "")
      remarks <- configuration.remarksConverter.additionalDcmi(metadata / "emd.xml", datasetId, fromVault)
      amdFile = metadata / "amd.xml"
      amdIn <- getAmdXml(datasetId, amdFile)
      hasSensitiveData = (amdIn \\ "containsPrivacySensitiveData").text
      ddmOut <- configuration.ddmTransformer.transform(ddmIn, datasetId, remarks, hasSensitiveData)
      _ = if (bagDir.isHidden && (amdIn \\ "datasetState").text != "DELETED")
        throw InvalidBagException(s"Inactive bag does not have state DELETED: $amdFile")
      amdOut <- configuration.amdTransformer.transform(amdIn, ddmOut \ "profile" \ "created")
      agreementsFile = metadata / "depositor-info" / "agreements.xml"
      _ = checkAgreementsXml((amdOut \ "depositorId").text, agreementsFile)
      provenanceXml = provenance.collectChangesInXmls(Seq(
        compareAMD(amdIn, amdOut),
        compareDDM(ddmIn, ddmOut),
        Provenance.fixedDdmEncoding(oldDdmChars, newDdmChars),
      ))
      _ = trace(bagInfo)
      _ = bagInfoKeysToRemove.foreach(mutableBagMetadata.remove)
      _ = depositProps.setProperty("depositor.userId", (amdOut \ "depositorId").text)
      // so far collecting changes
      _ = depositProps.save((bagParentDir / "deposit.properties").toJava) // N.B. the first write action
      _ = ddmFile.writeText(ddmOut.serialize)
      _ = amdFile.writeText(amdOut.serialize)
      _ = (metadata / "provenance.xml").writeText(provenanceXml.serialize)
      _ = trace("updating metadata")
      _ <- BagFacade.updateMetadata(bag)
      _ = trace("updating payload manifest")
      _ <- copyMigrationFiles(metadata, migration, fromVault)
      _ <- BagFacade.updatePayloadManifests(bag, Paths.get("data/easy-migration"))
      _ = trace("writing payload manifests")
      _ <- BagFacade.writePayloadManifests(bag)
      _ = trace("updating tag manifest")
      _ <- BagFacade.updateTagManifests(bag)
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
      logger.error(s"${ bagParentDir.name } failed with not expected error: ${ e.getClass.getSimpleName } ${ e.getMessage }", e)
      Failure(e)
  }

  private def getAmdXml(datasetId: String, amdFile: File): Try[Node] = {
    if (amdFile.exists)
      loadXml(amdFile).map(_._1)
    else {
      configuration.maybeFedoraProvider.map { provider =>
        provider.loadFoXml(datasetId).flatMap(getAmd)
      }.getOrElse(Failure(new IllegalStateException(s"no AMD for $datasetId and no fedora configured")))
    }
  }



  private def checkAgreementsXml(depositorId: String, agreementsFile: File) = {
    if (agreementsFile.exists) { //the agreementsfile is created by easy-fedora-to-bag for FEDORA datasets
      Success
    }
    else {
      val templateFile: File = configuration.agreementsPath / (depositorId + "-agreements.xml")
      if (!templateFile.exists) {
        Failure(new FileNotFoundException(templateFile + " not found"))
      }
      else {
        agreementsFile.parent.createIfNotExists(true)
        templateFile.copyTo(agreementsFile)
      }
    }
  }


  private def copyMigrationFiles(metadata: File, migration: File, fromVault: Boolean): Try[Unit] = Try {
    trace(metadata, migration)
    val filesXmlFile = (metadata / "files.xml").toString()
    val migrationFiles = {if (fromVault) Seq("provenance.xml", "dataset.xml", "files.xml") else Seq("provenance.xml", "dataset.xml", "files.xml", "emd.xml")}
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
    val children = bagParentDir.list.toList
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
