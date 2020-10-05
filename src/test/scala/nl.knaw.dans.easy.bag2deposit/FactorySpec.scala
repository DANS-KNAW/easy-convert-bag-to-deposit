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

import java.io.{ StringWriter, Writer }
import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.bag2deposit.Fixture.{ AppConfigSupport, BagIndexSupport }
import nl.knaw.dans.lib.error._
import org.apache.commons.configuration.PropertiesConfiguration
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success
import scala.xml.XML

class FactorySpec extends AnyFlatSpec with Matchers with AppConfigSupport with BagIndexSupport {

  "create" should "not call the bag-index" in {
    val bag = File("src/test/resources/bags/01") / "04e638eb-3af1-44fb-985d-36af12fccb2d" / "bag-revision-1"
    DepositPropertiesFactory(mockedConfig(null))
      .create(
        BagInfo(bag / "bag-info.txt").unsafeGetOrThrow,
        ddm = XML.loadFile((bag / "metadata" / "dataset.xml").toJava),
        IdType.DOI,
      ).map(serialize) shouldBe Success(
      """state.label = SUBMITTED
        |state.description = This deposit was extracted from the vault and is ready for processing
        |deposit.origin = vault
        |creation.timestamp = 2016-06-07
        |depositor.userId = user001
        |bag-store.bag-id = 04e638eb-3af1-44fb-985d-36af12fccb2d
        |bag-store.bag-name = bag-revision-1
        |identifier.doi = 10.5072/dans-2xg-umq8
        |identifier.urn = urn:nbn:nl:ui:13-00-3haq
        |identifier.fedora = easy-dataset:162288
        |dataverse.bag-id = urn:uuid:04e638eb-3af1-44fb-985d-36af12fccb2d
        |dataverse.sword-token = urn:uuid:04e638eb-3af1-44fb-985d-36af12fccb2d
        |dataverse.nbn = urn:uuid:urn:nbn:nl:ui:13-00-3haq
        |dataverse.identifier = 10.5072/dans-2xg-umq8
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
    val bagInfo = BagInfo(
      uuid = bagUUID,
      versionOf = Some(baseUUID),
      userId = "user001",
      created = "2017-01-16T14:35:00.888+01:00",
      bagName = "bag-name",
    )
    val ddm = <ddm:DDM xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                <ddm:dcmiMetadata>
                  <dcterms:identifier xsi:type="id-type:DOI">10.5072/dans-2xg-umq8</dcterms:identifier>
                  <dcterms:identifier xsi:type="id-type:URN">urn:nbn:nl:ui:13-00-3haq</dcterms:identifier>
                  <dcterms:identifier>easy-dataset:162288</dcterms:identifier>
                </ddm:dcmiMetadata>
              </ddm:DDM>

    DepositPropertiesFactory(mockedConfig(mockBagIndexRespondsWith(bagIndexBody, 200)))
      .create(bagInfo, ddm, IdType.URN)
      .map(serialize) shouldBe Success(
      s"""state.label = SUBMITTED
         |state.description = This deposit was extracted from the vault and is ready for processing
         |deposit.origin = vault
         |creation.timestamp = 2017-01-16T14:35:00.888+01:00
         |depositor.userId = user001
         |bag-store.bag-id = $bagUUID
         |bag-store.bag-name = bag-name
         |identifier.doi = 10.5072/dans-2xg-umq8
         |identifier.urn = urn:nbn:nl:ui:13-00-3haq
         |identifier.fedora = easy-dataset:162288
         |dataverse.bag-id = urn:uuid:$bagUUID
         |dataverse.sword-token = urn:uuid:$baseUUID
         |dataverse.nbn = urn:uuid:urn:nbn:nl:ui:13-z4-f8cm
         |dataverse.identifier = urn:nbn:nl:ui:13-00-3haq
         |""".stripMargin
    )
  }
  it should "create dataverse.other-id" in {
    val bagUUID = UUID.randomUUID()
    val bagInfo = BagInfo(
      uuid = bagUUID,
      versionOf = None,
      userId = "user001",
      created = "2017-01-16T14:35:00.888+01:00",
      bagName = "bag-name",
    )
    val ddm = <ddm:DDM xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                <ddm:dcmiMetadata>
                  <dcterms:identifier xsi:type="id-type:DOI">10.12345/foo-bar</dcterms:identifier>
                  <dcterms:identifier xsi:type="id-type:URN">urn:nbn:nl:ui:13-00-3haq</dcterms:identifier>
                  <dcterms:identifier>easy-dataset:162288</dcterms:identifier>
                </ddm:dcmiMetadata>
              </ddm:DDM>

    DepositPropertiesFactory(mockedConfig(null))
      .create(bagInfo, ddm, IdType.URN)
      .map(serialize) shouldBe Success(
      s"""state.label = SUBMITTED
         |state.description = This deposit was extracted from the vault and is ready for processing
         |deposit.origin = vault
         |creation.timestamp = 2017-01-16T14:35:00.888+01:00
         |depositor.userId = user001
         |bag-store.bag-id = $bagUUID
         |bag-store.bag-name = bag-name
         |identifier.doi = 10.12345/foo-bar
         |identifier.urn = urn:nbn:nl:ui:13-00-3haq
         |identifier.fedora = easy-dataset:162288
         |dataverse.bag-id = urn:uuid:$bagUUID
         |dataverse.sword-token = urn:uuid:$bagUUID
         |dataverse.nbn = urn:uuid:urn:nbn:nl:ui:13-00-3haq
         |dataverse.other-id = 10.12345/foo-bar
         |dataverse.identifier = urn:nbn:nl:ui:13-00-3haq
         |""".stripMargin
    )
  }

  private def serialize(configuration: PropertiesConfiguration) = {
    val writer: Writer = new StringWriter()
    configuration.save(writer)
    writer.toString
  }
}
