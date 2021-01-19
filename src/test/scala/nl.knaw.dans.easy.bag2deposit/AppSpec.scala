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
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.bag.v0.DansV0Bag.EASY_USER_ACCOUNT_KEY
import nl.knaw.dans.easy.bag2deposit.BagSource._
import nl.knaw.dans.easy.bag2deposit.Fixture.{ AppConfigSupport, FileSystemSupport }
import nl.knaw.dans.easy.bag2deposit.IdType._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalaj.http.HttpResponse

import scala.util.Success

class AppSpec extends AnyFlatSpec with Matchers with AppConfigSupport with FileSystemSupport {
  private val validUUID = "04e638eb-3af1-44fb-985d-36af12fccb2d"
  private val srcDir = testDir / "exports" / validUUID / "bag-revision-1"
  private val targetDir = testDir / "ingest-dir" / validUUID / "bag-revision-1"

  "addPropsToBags" should "alter original deposit dir" in {
    copyBags(Array(File("src/test/resources/bags/01") / validUUID))

    // pre conditions
    srcDir / ".." / "deposit.properties" shouldNot exist
    (srcDir / "bag-info.txt").contentAsString should include(EASY_USER_ACCOUNT_KEY)
    val manifestContent = (srcDir / "tagmanifest-sha1.txt").contentAsString

    val appConfig = testConfig(null)
    new EasyConvertBagToDepositApp(appConfig).addPropsToBags(
      (testDir / "exports").children,
      None,
      DepositPropertiesFactory(appConfig, URN, FEDORA)
    ) shouldBe Success("No fatal errors")

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

    val delegate = mock[MockBagIndex]
    val noBaseBagUUID = "87151a3a-12ed-426a-94f2-97313c7ae1f2"
    (delegate.execute(_: String)) expects s"/bag-sequence?contains=$validUUID" returning
      new HttpResponse[String]("123", 200, Map.empty)
    (delegate.execute(_: String)) expects s"/bag-sequence?contains=$noBaseBagUUID" returning
      new HttpResponse[String]("123", 200, Map.empty)
    (delegate.execute(_: String)) expects s"/bags/4722d09d-e431-4899-904c-0733cd773034" returning
      new HttpResponse[String]("<result><bag-info><urn>urn:nbn:nl:ui:13-z4-f8cm</urn><doi>10.5072/dans-2xg-umq8</doi></bag-info></result>", 200, Map.empty)
    val appConfig = testConfig(delegatingBagIndex(delegate))

    new EasyConvertBagToDepositApp(appConfig).addPropsToBags(
      (testDir / "exports").children,
      maybeOutputDir = Some((testDir / "ingest-dir").createDirectories()),
      DepositPropertiesFactory(appConfig, DOI, VAULT)
    ) shouldBe Success("No fatal errors")

    // post condition
    (targetDir / ".." / "deposit.properties").contentAsString should include("dataverse.id-identifier = dans-2xg-umq8")
    // other details verified in other test, note that the DOI has no prefix

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
