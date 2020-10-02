package nl.knaw.dans.easy.bag2deposit

import java.io.{ StringWriter, Writer }
import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.bag2deposit.Fixture.{ AppConfigSupport, FileSystemSupport }
import nl.knaw.dans.easy.bag2deposit.IdType.IdType
import nl.knaw.dans.lib.error._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success
import scala.xml.XML

class FactorySpec extends AnyFlatSpec with Matchers with AppConfigSupport with FileSystemSupport {

  private def callCreate(bag: File, writer: Writer, config: Configuration, doi: IdType) = DepositPropertiesFactory(config)
    .create(
      BagInfo(bag / "bag-info.txt").unsafeGetOrThrow,
      ddm = XML.loadFile((bag / "metadata" / "dataset.xml").toJava),
      doi,
    ).map(_.save(writer))

  private val bags = File("src/test/resources/bags/01")

  "create" should "not call the bag-index" in {
    val bag = bags / "04e638eb-3af1-44fb-985d-36af12fccb2d" / "bag-revision-1"
    val writer: Writer = new StringWriter()
    callCreate(bag, writer, mockedConfig, IdType.DOI) shouldBe a[Success[_]]
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
    val uuid1 = UUID.randomUUID()
    val uuid2 = UUID.randomUUID()
    val bag = (testDir / s"$uuid1" / "bag-name" / "metadata").createDirectories().parent
    (bag / "bag-info.txt").write(
      s"""EASY-User-Account: user001
         |Bagging-Date: 2017-01-16T14:35:00.888+01:00
         |Is-Version-Of: urn:uuid:$uuid1
         |""".stripMargin
    )
    (bag / "metadata" / "dataset.xml").write(
      """<ddm:DDM  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        |  <ddm:dcmiMetadata>
        |    <dcterms:identifier xsi:type="id-type:DOI">10.5072/dans-2xg-umq8</dcterms:identifier>
        |    <dcterms:identifier xsi:type="id-type:URN">urn:nbn:nl:ui:13-00-3haq</dcterms:identifier>
        |    <dcterms:identifier>easy-dataset:162288</dcterms:identifier>
        |  </ddm:dcmiMetadata>
        |</ddm:DDM>
        |""".stripMargin
    )
    val config = mockedConfig
    (config.bagIndex.get(_: UUID)) expects uuid1 returning Success(
      BagIndexInfo(uuid1, uuid2, doi = "", urn = "blabla")
    )
    val writer: Writer = new StringWriter()
    callCreate(bag, writer, config, IdType.DOI) shouldBe a[Success[_]]
    writer.toString shouldBe
      s"""state.label = SUBMITTED
        |state.description = This deposit was extracted from the vault and is ready for processing
        |deposit.origin = vault
        |creation.timestamp = 2017-01-16T14:35:00.888+01:00
        |depositor.userId = user001
        |bag-store.bag-id = $uuid1
        |bag-store.bag-name = bag-name
        |identifier.doi = 10.5072/dans-2xg-umq8
        |identifier.urn = urn:nbn:nl:ui:13-00-3haq
        |identifier.fedora = easy-dataset:162288
        |dataverse.bag-id = urn:uuid:$uuid1
        |dataverse.sword-token = urn:uuid:$uuid2
        |dataverse.nbn = urn:uuid:blabla
        |dataverse.identifier = 10.5072/dans-2xg-umq8
        |""".stripMargin
  }
}
