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
import nl.knaw.dans.easy.bag2deposit.Fixture.FileSystemSupport
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class AppSpec extends AnyFlatSpec with Matchers with FileSystemSupport with MockFactory {
  "createSips" should "" in {
    val appConfig = mockedConfig
    File("src/test/resources/bags/01").children.toArray.foreach { testBag =>
      testBag.copyTo(
        (testDir / "exports" / testBag.name).createDirectories()
      )
    }
    new EasyVaultExportIpApp(appConfig).addPropsToBags(
      (testDir / "exports").children,
      IdType.DOI,
      None,
      DepositPropertiesFactory(appConfig)
    ) shouldBe Success("See logging")
    testDir / "exports" / "04e638eb-3af1-44fb-985d-36af12fccb2d" / "deposit.properties" should exist
  }

  private def mockedConfig = {
    new Configuration(
      version = "testVersion",
      dansDoiPrefixes = Seq("10.17026/", "10.5072/"),
      bagIndex = mock[BagIndex],
    )
  }
}
