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

import java.util.UUID

import better.files.File
import better.files.File.CopyOptions
import nl.knaw.dans.easy.bagstore.BagId
import nl.knaw.dans.easy.v2ip.Command.FeedBackMessage
import nl.knaw.dans.easy.v2ip.IdType.IdType
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Failure, Success, Try }

class EasyVaultExportIpApp(configuration: Configuration) extends DebugEnhancedLogging {
  private val stores = configuration.storesComponent.bagStores
  def createSips(bagIds: Iterator[UUID], idType: IdType, outputDir: File, logFile: File): Try[FeedBackMessage] = {
    logger.info(s"creating SIPs: $idType, $outputDir, $logFile")
    bagIds.foreach { bagUuid =>
      val sipDir = configuration.stagingDir / UUID.randomUUID().toString
      val bagCopyDir = (sipDir / bagUuid.toString).path
      for {
        (path, store) <- stores.copyToDirectory(BagId(bagUuid), bagCopyDir)
        _ = logger.info(s"creating deposit.properties for $path")
        _ = (File(path.getParent) / "deposit.properties").writeText("")
        _ = logger.info(s"moving to $path")
        _ = sipDir.moveTo(outputDir / sipDir.name)(CopyOptions.atomically)
      } yield ()
    }
    Success("no fatal errors")
  }
}
