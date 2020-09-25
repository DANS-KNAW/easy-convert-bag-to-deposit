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

import java.nio.file.Paths
import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.bagstore.component._
import nl.knaw.dans.easy.bagstore.{ BagFacadeComponent, BagId, BaseDir }
import nl.knaw.dans.easy.v2ip.Command.FeedBackMessage
import nl.knaw.dans.easy.v2ip.IdType.IdType
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.collection.JavaConverters._
import scala.util.Try

class EasyVaultExportIpApp(configuration: Configuration) {

  val wired: BagStoresComponent = new BagStoresComponent
    with FileSystemComponent
    with BagProcessingComponent
    with BagStoreComponent
    with BagFacadeComponent
    with DebugEnhancedLogging {

    override lazy val fileSystem: FileSystem = ???
    override lazy val bagProcessing: BagProcessing = ???
    override lazy val bagFacade: BagFacade = ???
    override lazy val bagStores: BagStores = new BagStores {
      override def storeShortnames: Map[String, BaseDir] = {
        val stores = configuration.stores
        stores.getKeys.asScala
          .map(name => name -> Paths.get(stores.getString(name)).toAbsolutePath)
          .toMap
      }
    }
  }

  def createSips(ids: Iterator[UUID], idType: IdType, outDir: File, logFile: File): Try[FeedBackMessage] = {
    ids.map { uuid =>
      val sipDir = outDir / uuid.toString
      wired.bagStores.copyToDirectory(BagId(uuid), sipDir.path)
      // TODO create (sipDir / "deposit.properties")
    }
    ???
  }
}
