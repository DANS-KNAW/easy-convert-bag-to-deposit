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
import nl.knaw.dans.easy.bag2deposit.IdType._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class AppSpec extends AnyFlatSpec with Matchers with AppConfigSupport with FileSystemSupport with BagIndexSupport {
  private val validUUID = "04e638eb-3af1-44fb-985d-36af12fccb2d"
  private val srcDir = testDir / "exports" / validUUID / "bag-revision-1"
  private val targetDir = testDir / "ingest-dir" / validUUID / "bag-revision-1"

  "addPropsToBags" should "alter original deposit dir" in {
    copyBags(Array(File("src/test/resources/bags/01") / validUUID))

    // pre conditions
    srcDir / ".." / "deposit.properties" shouldNot exist
    (srcDir / "bag-info.txt").contentAsString should include(EASY_USER_ACCOUNT_KEY)
    val manifestContent = (srcDir / "tagmanifest-sha1.txt").contentAsString

    val appConfig = mockedConfig(null)
    new EasyConvertBagToDespositApp(appConfig).addPropsToBags(
      (testDir / "exports").children,
      None,
      DepositPropertiesFactory(appConfig, URN, FEDORA)
    ) shouldBe Success("See logging")

    // post conditions
    (testDir / "exports").children.toList.size shouldBe 1
    srcDir / ".." / "deposit.properties" should exist // content verified in FactorySpec
    (srcDir / "bag-info.txt").contentAsString shouldNot include(EASY_USER_ACCOUNT_KEY)
    (srcDir / "tagmanifest-sha1.txt") should not be manifestContent
  }

  it should "move only the valid export" in {
    copyBags(File("src/test/resources/bags/01").children.toArray)

    // pre condition
    srcDir / ".." / "deposit.properties" shouldNot exist

    val appConfig = mockedConfig(mockBagIndexRespondsWith("", 404)) // base bag not found
    new EasyConvertBagToDespositApp(appConfig).addPropsToBags(
      (testDir / "exports").children,
      maybeOutputDir = Some((testDir / "ingest-dir").createDirectories()),
      DepositPropertiesFactory(appConfig, DOI, VAULT)
    ) shouldBe Success("See logging")

    // post condition
    targetDir / ".." / "deposit.properties" should exist
    // other changes verified in other test

    // TODO (manually) intercept logging: the bag names should reflect the errors
    //  no variation in bag-info.txt not found or a property in that file not found
  }

  private def copyBags(bagsToProcess: Array[File]): Unit = {
    bagsToProcess.foreach { testBag =>
      testBag.copyTo(
        (testDir / "exports" / testBag.name).createDirectories()
      )
    }
  }
}
