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
import nl.knaw.dans.easy.v2ip.Fixture.FileSystemSupport
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class AppSpec extends AnyFlatSpec with Matchers with FileSystemSupport with MockFactory {
  private val app = new EasyVaultExportIpApp(new Configuration("testVersion", Seq("10.17026/","10.5072/")))
  "createSips" should "" in {
    File("src/test/resources/bags/01").children.toArray.foreach { testBag =>
      testBag.copyTo(
        (testDir / "exports" / testBag.name).createDirectories()
      )
    }
    app.addPropsToSips(
      (testDir / "exports").children,
      IdType.DOI,
      None,
      DepositProperties.default()
    ) shouldBe Success("See logging")
    testDir / "exports" / "04e638eb-3af1-44fb-985d-36af12fccb2d" / "deposit.properties" should exist
  }
}
