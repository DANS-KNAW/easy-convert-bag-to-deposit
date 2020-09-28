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

import better.files.File
import better.files.File.CopyOptions
import nl.knaw.dans.easy.v2ip.Command.FeedBackMessage
import nl.knaw.dans.easy.v2ip.IdType.IdType
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Failure, Success, Try }
import scala.xml.XML

class EasyVaultExportIpApp(configuration: Configuration) extends DebugEnhancedLogging {

  def addPropsToSips(sipDirs: Iterator[File], idType: IdType, outputDir: File, logFile: File): Try[FeedBackMessage] = {
    logger.info(s"creating SIPs ${ configuration.version }: $idType, $sipDirs, $logFile")
    sipDirs.foreach { sipDir =>
      for {
        metadataDir <- getMetadataDir(sipDir)
        bagInfo <- BagInfo(metadataDir / "bag-info.txt")
        ddm = XML.loadFile((metadataDir / "datasetxml").toJava)
        props <- DepositProperties(bagInfo, ddm)
        _ = props.save((sipDir / "deposit.properties").toJava)
        target = outputDir / sipDir.name
        _ = logger.info(s"moving $sipDir to $target")
        _ = sipDir.moveTo(target)(CopyOptions.atomically)
      } yield ()
    }
    Success("no fatal errors")
  }

  private def getMetadataDir(sipDir: File): Try[File] = {
    def fail(prefix: String) = Failure(new IllegalArgumentException(
      s"$prefix */metadata directory found in ${ sipDir.toJava.getAbsolutePath }")
    )

    val dirs = sipDir.children.flatMap(_.children.filter(dir => dir.isDirectory && dir.name == "metadata")).toList
    if (dirs.size > 1) fail("more than one")
    else dirs.map(Success(_)).headOption.getOrElse(fail("no"))
  }
}
