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
import nl.knaw.dans.bag.v0.DansV0Bag.EASY_USER_ACCOUNT_KEY
import nl.knaw.dans.easy.bag2deposit.BagSource._
import nl.knaw.dans.easy.bag2deposit.Fixture.{ AppConfigSupport, BagIndexSupport, FileSystemSupport }
import nl.knaw.dans.easy.bag2deposit.IdType.DOI
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{ Failure, Success }

class AppSpec extends AnyFlatSpec with Matchers with AppConfigSupport with FileSystemSupport with BagIndexSupport {
  "addPropsToBags" should "log all kind of io errors" in {
    val appConfig = mockedConfig(null)
    File("src/test/resources/bags/01").children.toArray.foreach { testBag =>
      testBag.copyTo(
        (testDir / "exports" / testBag.name).createDirectories()
      )
    }
    new EasyConvertBagToDespositApp(appConfig).addPropsToBags(
      (testDir / "exports").children,
      None,
      DepositPropertiesFactory(appConfig, DOI, VAULT)
    ) shouldBe Success("See logging")
    testDir / "exports" / "04e638eb-3af1-44fb-985d-36af12fccb2d" / "deposit.properties" should exist
  }
  it should "move the completed deposit" in {
    // valid test bag
    val uuid = "04e638eb-3af1-44fb-985d-36af12fccb2d"

    File("src/test/resources/bags/01/" + uuid).copyTo(
      (testDir / "exports" / uuid).createDirectories()
    )
    val srcBagDir = testDir / "exports" / uuid / "bag-revision-1"
    val ingestBagDir = testDir / "ingest-dir" / uuid / "bag-revision-1"

    // preconditions
    srcBagDir / ".." / "deposit.properties" shouldNot exist
    (srcBagDir / "bag-info.txt").contentAsString should include(EASY_USER_ACCOUNT_KEY)
    val manifestContent = (srcBagDir / "tagmanifest-sha1.txt").contentAsString

    val appConfig = mockedConfig(null)
    new EasyConvertBagToDespositApp(appConfig).addPropsToBags(
      (testDir / "exports").children,
      maybeOutputDir = Some((testDir / "ingest-dir").createDirectories()),
      DepositPropertiesFactory(appConfig, DOI, FEDORA)
    ) shouldBe Success("See logging")

    // post conditions
    (testDir / "exports").children shouldBe empty
    ingestBagDir / ".." / "deposit.properties" should exist
    (ingestBagDir / "bag-info.txt").contentAsString shouldNot include(EASY_USER_ACCOUNT_KEY)
    (ingestBagDir / "tagmanifest-sha1.txt") should not be manifestContent
  }
  it should "complain about not existing bag-info.txt" in {
    val uuid = "42c8dcbe-df51-422e-9c7e-9bd7a0b1ecc0"
    File("src/test/resources/bags/01/" + uuid).copyTo(
      (testDir / "exports" / uuid).createDirectories()
    )
    val appConfig = mockedConfig(null)
    new EasyConvertBagToDespositApp(appConfig).addPropsToBags(
      (testDir / "exports").children,
      maybeOutputDir = Some((testDir / "ingest-dir").createDirectories()),
      DepositPropertiesFactory(appConfig, DOI, FEDORA)
    ) shouldBe Success("See logging")

    (testDir / "exports" / uuid) should exist
    (testDir / "ingest-dir").children shouldBe empty // deposit was not moved
    // see logging manually: Error ... <uuid> failed ... NoSuchFileException ... bag-it.txt,
  }

}
