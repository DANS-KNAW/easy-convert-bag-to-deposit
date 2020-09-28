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

import java.nio.file.{ Path, Paths }
import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.bagstore.component._
import nl.knaw.dans.easy.bagstore.{ BagFacadeComponent, BagId, BaseDir, ItemId }
import nl.knaw.dans.easy.v2ip.Fixture.FileSystemSupport
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class AppSpec extends AnyFlatSpec with Matchers with FileSystemSupport with MockFactory {
  "createSips" should "" in {
    val bags = File("src/test/resources/bags").children.toArray.map { testBag =>
      testBag.copyTo(
        (testDir / "staging" / UUID.randomUUID().toString / UUID.randomUUID().toString).createDirectories()
      )
    }
    newApp(mockedBagStoreComponent).createSips(
      bags.map(f => UUID.fromString(f.name)).toIterator,
      IdType.DOI,
      testDir / "output",
      testDir / "log.csv"
    ) shouldBe Success("no fatal errors")
    bags.map(_.parent).map(_ / "deposit.properties").foreach(_ should exist)
  }

  private def newApp(bagStoresComponent: BagStoresComponent) = {
    val configuration = new Configuration("testVersion", testDir / "staging", bagStoresComponent)
    new EasyVaultExportIpApp(configuration)
  }

  /** expecting copyToDirectory calls for each testDir/staging */
  private def mockedBagStoreComponent: BagStoresComponent = {
    val bagStoresComponent: BagStoresComponent = new BagStoresComponent
      with FileSystemComponent
      with BagProcessingComponent
      with BagStoreComponent
      with BagFacadeComponent
      with DebugEnhancedLogging {

      override lazy val fileSystem: FileSystem = ???
      override lazy val bagProcessing: BagProcessing = ???
      override lazy val bagFacade: BagFacade = ???
      override lazy val bagStores: BagStores = mock[BagStores]
    }
    (testDir / "staging").children.foreach { sipDir: File =>
      val bagDir = sipDir.children.toSeq.head
      val bagId = BagId(UUID.fromString(bagDir.name))
      (bagStoresComponent.bagStores.copyToDirectory(
        _: ItemId, _: Path, _: Boolean, _: Option[BaseDir], _: Boolean
      )) expects(*, *, *, *, *) onCall { (bagId, output, _,_,_) =>
        Success(bagDir.path, Paths.get("store-name"))
      }
    }
    bagStoresComponent
  }
}
