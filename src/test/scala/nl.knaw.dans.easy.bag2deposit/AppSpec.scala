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
import nl.knaw.dans.easy.bag2deposit.Fixture.{ AppConfigSupport, FileSystemSupport }
import nl.knaw.dans.easy.bag2deposit.IdType._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalaj.http.HttpResponse

import scala.util.Success

class AppSpec extends AnyFlatSpec with Matchers with AppConfigSupport with FileSystemSupport {
  private val resourceBags: File = File("src/test/resources/bags/01")
  private val validUUID = "04e638eb-3af1-44fb-985d-36af12fccb2d"

  "addPropsToBags" should  "move valid exports" in {
    val delegate = mock[MockBagIndex]
    val noBaseBagUUID = "87151a3a-12ed-426a-94f2-97313c7ae1f2"
    (delegate.execute(_: String)) expects s"bag-sequence?contains=$validUUID" returning
      new HttpResponse[String]("123", 200, Map.empty)
    (delegate.execute(_: String)) expects s"bag-sequence?contains=$noBaseBagUUID" returning
      new HttpResponse[String]("123", 200, Map.empty)
    (delegate.execute(_: String)) expects s"bags/4722d09d-e431-4899-904c-0733cd773034" returning
      new HttpResponse[String]("<result><bag-info><urn>urn:nbn:nl:ui:13-z4-f8cm</urn><doi>10.5072/dans-2xg-umq8</doi></bag-info></result>", 200, Map.empty)
    val appConfig = testConfig(delegatingBagIndex(delegate))

    (resourceBags.children.toArray).foreach { testBag =>
      testBag.copyTo(
        (testDir / "exports" / testBag.name).createDirectories()
      )
    }

    //////// end of mocking and preparations

    new EasyConvertBagToDepositApp(appConfig).addPropsToBags(
      (testDir / "exports").children,
      maybeOutputDir = Some((testDir / "ingest-dir").createDirectories()),
      DepositPropertiesFactory(appConfig, DOI, VAULT)
    ) shouldBe Success("No fatal errors")

    //////// general post conditions

    // TODO (manually) intercept logging: the bag names should reflect the errors
    //  no variation in bag-info.txt not found or a property in that file not found

    val movedDirs = (testDir / "ingest-dir").children.toList
    val leftDirs = (testDir / "exports").children.toList

    // only moved dirs changed
    leftDirs.foreach(dir => dir.isSameContentAs(resourceBags / dir.name) shouldBe true)
    movedDirs.foreach(dir => dir.isSameContentAs(resourceBags / dir.name) shouldBe false)

    // total number of deposits should not change
    movedDirs.size + leftDirs.size shouldBe  resourceBags.children.toList.size

    movedDirs.size shouldBe 2 // base-bag-not-found is moved together with the valid bag-revision-1
    // TODO should addPropsToBags check existence of base-bag in case of versioned bags?
    //  If so, can the base-bag have been moved to ingest-dir while processing?

    //////// changes made to the valid bag

    val validBag = resourceBags / validUUID / "bag-revision-1"
    val movedBag = testDir / "ingest-dir" / validUUID / "bag-revision-1"

    // content verified in BagInfoSpec
    validBag / ".." / "deposit.properties" shouldNot exist
    movedBag / ".." / "deposit.properties" should exist

    // other content changes verified in DepositPropertiesFactorySpec
    (validBag / "bag-info.txt").contentAsString should include(EASY_USER_ACCOUNT_KEY)
    (movedBag / "bag-info.txt").contentAsString shouldNot include(EASY_USER_ACCOUNT_KEY)

    // content of provenance verified in ddm.ProvenanceSpec
    (validBag / "tagmanifest-sha1.txt").contentAsString shouldNot include("metadata/provenance.xml")
    (movedBag / "tagmanifest-sha1.txt").contentAsString should include("metadata/provenance.xml")

    // other content changes are verified in ddm.*Spec
    (validBag / "metadata" / "dataset.xml").contentAsString should include("<dc:title>Example</dc:title>")
    (movedBag / "metadata" / "dataset.xml").contentAsString shouldNot include("<dc:title>Example</dc:title>")
  }
}
