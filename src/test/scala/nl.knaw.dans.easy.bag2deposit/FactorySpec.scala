package nl.knaw.dans.easy.bag2deposit

import java.io.{ StringWriter, Writer }
import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.bag2deposit.Fixture.AppConfigSupport
import nl.knaw.dans.lib.error._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success
import scala.xml.XML

class FactorySpec extends AnyFlatSpec with Matchers with AppConfigSupport {

  "create" should "not call the bag-index" in {
    val bag = File("src/test/resources/bags/01") / "04e638eb-3af1-44fb-985d-36af12fccb2d" / "bag-revision-1"
    val writer: Writer = new StringWriter()
    DepositPropertiesFactory(mockedConfig())
      .create(
        BagInfo(bag / "bag-info.txt").unsafeGetOrThrow,
        ddm = XML.loadFile((bag / "metadata" / "dataset.xml").toJava),
        IdType.DOI,
      ).map(_.save(writer)) shouldBe a[Success[_]]
    writer.toString shouldBe
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
  }
  it should "call the bag-index" in {
    val bagUUID = UUID.randomUUID()
    val baseUUID = UUID.randomUUID()
    val bagIndexInfo = BagIndexInfo(bagUUID, baseUUID, doi = "", urn = "blabla")
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

    val writer: Writer = new StringWriter()
    DepositPropertiesFactory(mockedConfig(bagIndexInfo))
      .create(bagInfo, ddm, IdType.DOI)
      .map(_.save(writer)) shouldBe a[Success[_]]
    writer.toString shouldBe
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
         |dataverse.nbn = urn:uuid:blabla
         |dataverse.identifier = 10.5072/dans-2xg-umq8
         |""".stripMargin
  }
}
