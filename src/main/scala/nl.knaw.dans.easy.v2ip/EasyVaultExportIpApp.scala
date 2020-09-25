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

import scala.util.{ Failure, Try }

class EasyVaultExportIpApp(configuration: Configuration) {
  private val stores = configuration.storesComponent.bagStores
  def createSips(ids: Iterator[UUID], idType: IdType, outputDir: File, logFile: File): Try[FeedBackMessage] = {
    val array = ids.toArray
    array.map { uuid =>
      val sipDir = configuration.stagingDir / uuid.toString
      for {
        (path, store) <- stores.copyToDirectory(BagId(uuid), sipDir.path)
        // TODO create sipDir/application.properties
        _ = sipDir.moveTo(outputDir / sipDir.name)(CopyOptions.atomically)
      } yield ()
    }
    Failure(new NotImplementedError(s"$outputDir/*/application.properties"))
  }
}
