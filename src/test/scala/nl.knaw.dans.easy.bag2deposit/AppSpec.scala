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
import nl.knaw.dans.easy.bag2deposit.BagSource._
import nl.knaw.dans.easy.bag2deposit.Fixture.{ AppConfigSupport, FileSystemSupport, XmlSupport }
import nl.knaw.dans.easy.bag2deposit.IdType._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalaj.http.HttpResponse

import scala.util.{ Success, Try }
import scala.xml.XML

class AppSpec extends AnyFlatSpec with XmlSupport with Matchers with AppConfigSupport with FileSystemSupport {
  private val resourceBags: File = File("src/test/resources/bags/01")
  private val validUUID = "04e638eb-3af1-44fb-985d-36af12fccb2d"

  "addPropsToBags" should "move valid exports" in {
    val delegate = mock[MockBagIndex]
    val noBaseBagUUID = "87151a3a-12ed-426a-94f2-97313c7ae1f2"
    (delegate.execute(_: String)) expects s"bag-sequence?contains=$validUUID" returning
      new HttpResponse[String]("123", 200, Map.empty)
    (delegate.execute(_: String)) expects s"bag-sequence?contains=$noBaseBagUUID" returning
      new HttpResponse[String]("123", 200, Map.empty)
    (delegate.execute(_: String)) expects s"bags/4722d09d-e431-4899-904c-0733cd773034" returning
      new HttpResponse[String]("<result><bag-info><urn>urn:nbn:nl:ui:13-z4-f8cm</urn><doi>10.5072/dans-2xg-umq8</doi></bag-info></result>", 200, Map.empty)
    val appConfig = testConfig(delegatingBagIndex(delegate), None)
    (appConfig.preStagedProvider.get(_: String, _: Int)) expects (*,1) returning Success(Seq.empty)
    resourceBags.copyTo(testDir / "exports")

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
    movedDirs.size + leftDirs.size shouldBe resourceBags.children.toList.size

    movedDirs.size shouldBe 2 // base-bag-not-found is moved together with the valid bag-revision-1
    // TODO should addPropsToBags check existence of base-bag in case of versioned bags?
    //  If so, can the base-bag have been moved to ingest-dir while processing?

    //////// changes made to the valid bag

    val validBag = resourceBags / validUUID / "bag-revision-1"
    val movedBag = testDir / "ingest-dir" / validUUID / "bag-revision-1"

    // DDM should have preserved its white space
    (movedBag / "metadata" / "dataset.xml").contentAsString should include
    """    <dcterms:description>An example of a dataset.
      |
      |    With another paragraph.
      |    </dcterms:description>""".stripMargin

    // content verified in BagInfoSpec
    validBag / ".." / "deposit.properties" shouldNot exist
    movedBag / ".." / "deposit.properties" should exist

    // other content changes verified in DepositPropertiesFactorySpec
    (validBag / "bag-info.txt").contentAsString should include(BagFacade.EASY_USER_ACCOUNT_KEY)
    (movedBag / "bag-info.txt").contentAsString shouldNot include(BagFacade.EASY_USER_ACCOUNT_KEY)

    // content of provenance verified in ddm.ProvenanceSpec
    (validBag / "tagmanifest-sha1.txt").contentAsString shouldNot include("metadata/provenance.xml")
    (movedBag / "tagmanifest-sha1.txt").contentAsString should include("metadata/provenance.xml")

    // other content changes are verified in ddm.*Spec
    (validBag / "metadata" / "dataset.xml").contentAsString should include("<dc:title>Example</dc:title>")
    (movedBag / "metadata" / "dataset.xml").contentAsString shouldNot include("<dc:title>Example</dc:title>")
    (validBag / "metadata" / "amd.xml").contentAsString should include("<depositorId>user001</depositorId>")
    (movedBag / "metadata" / "amd.xml").contentAsString should
      (include("<depositorId>USer</depositorId>") and not include "<depositorId>user001</depositorId>")
  }

  it should "load amd.xml from Fedora when not in the input bag" in {
    val delegate = mock[MockBagIndex]
    (delegate.execute(_: String)) expects s"bag-sequence?contains=$validUUID" returning
      new HttpResponse[String]("123", 200, Map.empty)

    val loadFoXmlResult =
      <foxml:digitalObject xsi:schemaLocation="info:fedora/fedora-system:def/foxml# http://www.fedora.info/definitions/1/0/foxml1-1.xsd" PID="easy-dataset:17" VERSION="1.1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:foxml="info:fedora/fedora-system:def/foxml#">
        <foxml:datastream VERSIONABLE="false" CONTROL_GROUP="X" STATE="A" ID="AMD">
          <foxml:datastreamVersion SIZE="4820" MIMETYPE="text/xml" CREATED="2021-06-04T12:06:56.477Z" LABEL="Administrative metadata for this dataset" ID="AMD.0">
            <foxml:xmlContent>
              <damd:administrative-md version="0.1" xmlns:damd="http://easy.dans.knaw.nl/easy/dataset-administrative-metadata/">
                <datasetState>SUBMITTED</datasetState>
                <previousState>DRAFT</previousState>
                <lastStateChange>2021-06-04T14:06:46.860+02:00</lastStateChange>
                <depositorId>easyadmin</depositorId>
                <stateChangeDates>
                  <damd:stateChangeDate>
                    <fromState>DRAFT</fromState>
                    <toState>SUBMITTED</toState>
                    <changeDate>2021-06-04T14:06:46.860+02:00</changeDate>
                  </damd:stateChangeDate>
                </stateChangeDates>
                <groupIds/>
              </damd:administrative-md>
            </foxml:xmlContent>
          </foxml:datastreamVersion>
        </foxml:datastream>
      </foxml:digitalObject>
    val getAmdResult =
      <damd:administrative-md version="0.1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:foxml="info:fedora/fedora-system:def/foxml#" xmlns:damd="http://easy.dans.knaw.nl/easy/dataset-administrative-metadata/">
        <datasetState>SUBMITTED</datasetState>
        <previousState>DRAFT</previousState>
        <lastStateChange>2021-06-04T14:06:46.860+02:00</lastStateChange>
        <depositorId>easyadmin</depositorId>
        <stateChangeDates>
          <damd:stateChangeDate>
            <fromState>DRAFT</fromState>
            <toState>SUBMITTED</toState>
            <changeDate>2021-06-04T14:06:46.860+02:00</changeDate>
          </damd:stateChangeDate>
        </stateChangeDates>
        <groupIds/>
      </damd:administrative-md>

    val fedoraProvider = mock[MockFedoraProvider]
    (fedoraProvider.loadFoXml _).expects("easy-dataset:162288").returning(Try(loadFoXmlResult)).once()
    val appConfig = testConfig(delegatingBagIndex(delegate), Option(fedoraProvider))
    (appConfig.preStagedProvider.get(_: String, _: Int)) expects (*,1) returning Success(Seq.empty)

    (resourceBags / validUUID).copyTo(testDir / "exports" / validUUID)
    (testDir / "exports" / validUUID / "bag-revision-1" / "metadata" / "amd.xml")
      .delete()

    new EasyConvertBagToDepositApp(appConfig).addPropsToBags(
      (testDir / "exports").children,
      maybeOutputDir = Some((testDir / "ingest-dir").createDirectories()),
      DepositPropertiesFactory(appConfig, DOI, VAULT)
    ) shouldBe Success("No fatal errors")

    val amdXml = testDir / "ingest-dir" / validUUID / "bag-revision-1/metadata/amd.xml"
    normalized(XML.loadFile(amdXml.toJava)) shouldBe normalized(getAmdResult)
  }

  it should "add the new data/easy-migration files into metadata/files.xml" in {
    val delegate = mock[MockBagIndex]
    (delegate.execute(_: String)) expects s"bag-sequence?contains=$validUUID" returning
      new HttpResponse[String]("123", 200, Map.empty)
    val appConfig = testConfig(delegatingBagIndex(delegate), null)
    (appConfig.preStagedProvider.get(_: String, _: Int)) expects (*,1) returning Success(Seq.empty)

    val originalFilesXml =
      <files xmlns:dcterms="http://purl.org/dc/terms/" xmlns="http://easy.dans.knaw.nl/schemas/bag/metadata/files/">
        <file filepath="data/leeg.txt">
          <dcterms:format>text/xml</dcterms:format>
        </file>
      </files>
    val expectedFilesXml =
      <files xmlns:dcterms="http://purl.org/dc/terms/" xmlns="http://easy.dans.knaw.nl/schemas/bag/metadata/files/">

        <file filepath="data/leeg.txt">
          <dcterms:format>text/xml</dcterms:format>
        </file>
        <file filepath="data/easy-migration/provenance.xml">
          <dcterms:format>text/xml</dcterms:format>
        </file>
        <file filepath="data/easy-migration/dataset.xml">
          <dcterms:format>text/xml</dcterms:format>
        </file>
        <file filepath="data/easy-migration/files.xml">
          <dcterms:format>text/xml</dcterms:format>
        </file>
        <file filepath="data/easy-migration/emd.xml">
          <dcterms:format>text/xml</dcterms:format>
        </file>
      </files>
    (resourceBags / validUUID).copyTo(testDir / "exports" / validUUID)
    (testDir / "exports" / validUUID / "bag-revision-1" / "metadata" / "files.xml")
      .writeText(originalFilesXml.serialize)

    new EasyConvertBagToDepositApp(appConfig).addPropsToBags(
      (testDir / "exports").children,
      maybeOutputDir = Some((testDir / "ingest-dir").createDirectories()),
      DepositPropertiesFactory(appConfig, DOI, VAULT)
    ) shouldBe Success("No fatal errors")

    val movedBag = testDir / "ingest-dir" / validUUID / "bag-revision-1"
    val filesXml = movedBag / "metadata" / "files.xml"
    normalized(XML.loadFile(filesXml.toJava)) shouldBe normalized(expectedFilesXml)
  }

  it should "produce proper prefix in new files.xml elements AND apply preferred user ID" in {
    val delegate = mock[MockBagIndex]
    (delegate.execute(_: String)) expects s"bag-sequence?contains=$validUUID" returning
      new HttpResponse[String]("123", 200, Map.empty)
    val appConfig = testConfig(delegatingBagIndex(delegate), null)
    (appConfig.preStagedProvider.get(_: String, _: Int)) expects (*,1) returning Success(Seq.empty)
    val originalFilesXml =
      <files xmlns:dc="http://purl.org/dc/terms/" xmlns="http://easy.dans.knaw.nl/schemas/bag/metadata/files/">
        <file filepath="data/leeg.txt">
          <dc:format>text/xml</dc:format>
        </file>
      </files>
    val expectedFilesXml =
      <files xmlns:dc="http://purl.org/dc/terms/" xmlns="http://easy.dans.knaw.nl/schemas/bag/metadata/files/">

        <file filepath="data/leeg.txt">
          <dc:format>text/xml</dc:format>
        </file>
        <file filepath="data/easy-migration/provenance.xml">
          <dc:format>text/xml</dc:format>
        </file>
        <file filepath="data/easy-migration/dataset.xml">
          <dc:format>text/xml</dc:format>
        </file>
        <file filepath="data/easy-migration/files.xml">
          <dc:format>text/xml</dc:format>
        </file>
        <file filepath="data/easy-migration/emd.xml">
          <dc:format>text/xml</dc:format>
        </file>
      </files>
    (resourceBags / validUUID).copyTo(testDir / "exports" / validUUID)
    val testBag = testDir / "exports" / validUUID / "bag-revision-1"
    (testBag / "metadata" / "files.xml")
      .writeText(originalFilesXml.serialize)

    // pre-condition: another user than in account-substitutes.csv
    (testBag / "metadata" / "amd.xml").contentAsString should
      include ("<depositorId>user001</depositorId>")

    new EasyConvertBagToDepositApp(appConfig).addPropsToBags(
      (testDir / "exports").children,
      maybeOutputDir = Some((testDir / "ingest-dir").createDirectories()),
      DepositPropertiesFactory(appConfig, DOI, VAULT)
    ) shouldBe Success("No fatal errors")

    val movedBag = testDir / "ingest-dir" / validUUID / "bag-revision-1"
    val filesXml = movedBag / "metadata" / "files.xml"
    normalized(XML.loadFile(filesXml.toJava)) shouldBe normalized(expectedFilesXml)

    // post-condition: user from account-substitutes.csv
    (movedBag / ".." / "deposit.properties").contentAsString should
      include("depositor.userId = USer")
    (movedBag / "metadata" / "amd.xml").contentAsString should
      include ("<depositorId>USer</depositorId>")
  }
}
