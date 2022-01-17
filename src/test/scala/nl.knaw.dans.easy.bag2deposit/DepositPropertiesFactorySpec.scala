/*
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

import java.io.{ StringWriter, Writer }
import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.bag2deposit.Fixture.{ AppConfigSupport, BagIndexSupport, BagSupport }
import nl.knaw.dans.lib.error._
import org.apache.commons.configuration.PropertiesConfiguration
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalaj.http.HttpResponse

import scala.util.Success
import scala.xml.XML

class DepositPropertiesFactorySpec extends AnyFlatSpec with Matchers with AppConfigSupport with BagIndexSupport with BagSupport with MockFactory {

  "create" should "call the bag-index for the sequence only" in {
    val uuid = "04e638eb-3af1-44fb-985d-36af12fccb2d"
    val bagDir = File("src/test/resources/bags/01") / uuid / "bag-revision-1"

    val delegate = mock[MockBagIndex]
    (delegate.execute(_: String)) expects s"bag-sequence?contains=$uuid" returning
      new HttpResponse[String](body = "123", code = 200, Map.empty)
    val cfg = testConfig(delegatingBagIndex(delegate), null)

    DepositPropertiesFactory(cfg, IdType.DOI, BagSource.VAULT)
      .create(
        BagInfo(bagDir, mockBag(bagDir).getMetadata).unsafeGetOrThrow,
        ddm = XML.loadFile((bagDir / "metadata" / "dataset.xml").toJava),
      ).map(serialize) shouldBe Success(
      s"""state.label = SUBMITTED
         |state.description = This deposit was extracted from the vault and is ready for processing
         |deposit.origin = VAULT
         |creation.timestamp = 2017-01-16T14:35:00.888+01:00
         |depositor.userId = user001
         |identifier.doi = 10.5072/dans-2xg-umq8
         |identifier.urn = urn:nbn:nl:ui:13-00-3haq
         |identifier.fedora = easy-dataset:162288
         |bag-store.bag-name = bag-revision-1
         |bag-store.bag-id = $uuid
         |dataverse.sword-token = $uuid
         |dataverse.nbn = urn:nbn:nl:ui:13-00-3haq
         |dataverse.bag-id = urn:uuid:$uuid
         |dataverse.id-protocol = doi
         |dataverse.id-identifier = dans-2xg-umq8
         |dataverse.id-authority = 10.80270
         |""".stripMargin
    )
  }

  it should "call the bag-index" in {
    val bagUUID = UUID.randomUUID()
    val baseUUID = UUID.randomUUID()
    val bagIndexBody =
      """<result>
        |    <bag-info>
        |        <bag-id>38cb3ff1-d59d-4560-a423-6f761b237a56</bag-id>
        |        <base-id>38cb3ff1-d59d-4560-a423-6f761b237a56</base-id>
        |        <created>2016-11-13T00:41:11.000+01:00</created>
        |        <doi>10.80270/test-28m-zann</doi>
        |        <urn>urn:nbn:nl:ui:13-z4-f8cm</urn>
        |    </bag-info>
        |</result>""".stripMargin
    val bagInfo = BagInfo(userId = "user001", created = "2017-01-16T14:35:00.888+01:00", uuid = bagUUID, bagName = "bag-name", versionOf = Some(baseUUID))
    val ddm = <ddm:DDM xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                <ddm:dcmiMetadata>
                  <dcterms:identifier xsi:type="id-type:DOI">10.5072/dans-2xg-umq8</dcterms:identifier>
                  <dcterms:identifier xsi:type="id-type:URN">urn:nbn:nl:ui:13-00-3haq</dcterms:identifier>
                  <dcterms:identifier>easy-dataset:162288</dcterms:identifier>
                  <dcterms:isFormatOf>PAN-00019180</dcterms:isFormatOf>
                  <dcterms:isFormatOf>pani</dcterms:isFormatOf>
                </ddm:dcmiMetadata>
              </ddm:DDM>

    val delegate = mock[MockBagIndex]
    (delegate.execute(_: String)) expects s"bags/$baseUUID" returning
      new HttpResponse[String](body = bagIndexBody, code = 200, Map.empty)
    (delegate.execute(_: String)) expects s"bag-sequence?contains=$bagUUID" returning
      new HttpResponse[String](body = "123", code = 200, Map.empty)
    val cfg = testConfig(delegatingBagIndex(delegate), null)

    DepositPropertiesFactory(cfg, IdType.URN, BagSource.VAULT)
      .create(bagInfo, ddm)
      .map(serialize) shouldBe Success(
      s"""state.label = SUBMITTED
         |state.description = This deposit was extracted from the vault and is ready for processing
         |deposit.origin = VAULT
         |creation.timestamp = 2017-01-16T14:35:00.888+01:00
         |depositor.userId = user001
         |identifier.doi = 10.5072/dans-2xg-umq8
         |identifier.urn = urn:nbn:nl:ui:13-00-3haq
         |identifier.fedora = easy-dataset:162288
         |bag-store.bag-name = bag-name
         |bag-store.bag-id = $bagUUID
         |dataverse.sword-token = $baseUUID
         |dataverse.nbn = urn:nbn:nl:ui:13-z4-f8cm
         |dataverse.other-id = PAN:00019180
         |dataverse.bag-id = urn:uuid:$bagUUID
         |dataverse.id-protocol = urn
         |dataverse.id-identifier = z4-f8cm
         |dataverse.id-authority = nbn:nl:ui:13
         |""".stripMargin
    )
  }

  it should "use base urn from bag-info.txt" in {
    val bagUUID = UUID.randomUUID()
    val baseUUID = UUID.randomUUID()
    val bagInfo = BagInfo(userId = "user001", created = "2017-01-16T14:35:00.888+01:00", uuid = bagUUID, bagName = "bag-name", versionOf = Some(baseUUID), basePids = Some(BasePids("urn:nbn:nl:ui:13-rabarbera", "testDoi")))
    val ddm = <ddm:DDM xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                <ddm:dcmiMetadata>
                  <dcterms:identifier xsi:type="id-type:DOI">10.5072/dans-2xg-umq8</dcterms:identifier>
                  <dcterms:identifier xsi:type="id-type:URN">urn:nbn:nl:ui:13-00-3haq</dcterms:identifier>
                  <dcterms:identifier>easy-dataset:162288</dcterms:identifier>
                </ddm:dcmiMetadata>
              </ddm:DDM>

    val cfg = testConfig(delegatingBagIndex(mock[MockBagIndex]), null)

    DepositPropertiesFactory(cfg, IdType.URN, BagSource.FEDORA)
      .create(bagInfo, ddm)
      .map(serialize) shouldBe Success(
      s"""state.label = SUBMITTED
         |state.description = This deposit was extracted from EASY-fedora and is ready for processing
         |deposit.origin = FEDORA
         |creation.timestamp = 2017-01-16T14:35:00.888+01:00
         |depositor.userId = user001
         |identifier.doi = 10.5072/dans-2xg-umq8
         |identifier.urn = urn:nbn:nl:ui:13-00-3haq
         |identifier.fedora = easy-dataset:162288
         |dataverse.nbn = urn:nbn:nl:ui:13-rabarbera
         |dataverse.bag-id = urn:uuid:$bagUUID
         |dataverse.id-protocol = urn
         |dataverse.id-identifier = rabarbera
         |dataverse.id-authority = nbn:nl:ui:13
         |""".stripMargin
    )
  }
  it should "create dataverse.other-id" in {
    val bagUUID = UUID.randomUUID()
    val bagInfo = BagInfo(userId = "user001", created = "2017-01-16T14:35:00.888+01:00", uuid = bagUUID, bagName = "bag-name", versionOf = None)
    val ddm = <ddm:DDM xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                <ddm:dcmiMetadata>
                  <dcterms:identifier xsi:type="id-type:DOI">10.12345/foo-bar</dcterms:identifier>
                  <dcterms:identifier xsi:type="id-type:URN">urn:nbn:nl:ui:13-00-3haq</dcterms:identifier>
                  <dcterms:identifier>easy-dataset:162288</dcterms:identifier>
                </ddm:dcmiMetadata>
              </ddm:DDM>

    val delegate = mock[MockBagIndex]
    (delegate.execute(_: String)) expects s"bag-sequence?contains=$bagUUID" returning
      new HttpResponse[String](body = "123", code = 200, Map.empty)
    val cfg = testConfig(delegatingBagIndex(delegate), null)

    DepositPropertiesFactory(cfg, IdType.URN, BagSource.VAULT)
      .create(bagInfo, ddm)
      .map(serialize) shouldBe Success(
      s"""state.label = SUBMITTED
         |state.description = This deposit was extracted from the vault and is ready for processing
         |deposit.origin = VAULT
         |creation.timestamp = 2017-01-16T14:35:00.888+01:00
         |depositor.userId = user001
         |identifier.doi = 10.12345/foo-bar
         |identifier.urn = urn:nbn:nl:ui:13-00-3haq
         |identifier.fedora = easy-dataset:162288
         |bag-store.bag-name = bag-name
         |bag-store.bag-id = $bagUUID
         |dataverse.sword-token = $bagUUID
         |dataverse.nbn = urn:nbn:nl:ui:13-00-3haq
         |dataverse.bag-id = urn:uuid:$bagUUID
         |dataverse.other-id = https://doi.org/10.12345/foo-bar
         |dataverse.id-protocol = urn
         |dataverse.id-identifier = 00-3haq
         |dataverse.id-authority = nbn:nl:ui:13
         |""".stripMargin
    )
  }

  private def serialize(configuration: PropertiesConfiguration) = {
    val writer: Writer = new StringWriter()
    configuration.save(writer)
    writer.toString
  }
}
