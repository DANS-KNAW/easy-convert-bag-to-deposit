package nl.knaw.dans.easy.v2ip

import java.nio.file.Paths
import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.bagstore.{ BagFacadeComponent, BaseDir, ConfigurationComponent }
import nl.knaw.dans.easy.bagstore.component.{ BagProcessingComponent, BagStoreComponent, BagStoreWiring, BagStoresComponent, FileSystemComponent }
import nl.knaw.dans.easy.v2ip.Fixture.FileSystemSupport
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class AppSpec extends AnyFlatSpec with Matchers with FileSystemSupport with MockFactory {
  "createSips" should "" in {
    File("src/test/resources/bags").children.toSeq.foreach(_.copyTo((testDir / "vault" / UUID.randomUUID().toString).createDirectories()))
    val uuids = (testDir / "vault").children.map(f => UUID.fromString(f.name))
    val bagStores = mockedBagStoreComponent
    //bagStores.copyToDirectory()
    newApp(bagStores)
      .createSips(uuids, IdType.DOI, testDir / "output", testDir / "log.csv") shouldBe Success("blabla")
  }

  private def newApp(bagStoresComponent: BagStoresComponent) = {
    val configuration = new Configuration("testVersion", testDir / "staging", bagStoresComponent)
    new EasyVaultExportIpApp(configuration)
  }

  private def mockedBagStoreComponent: BagStoresComponent = {
    new BagStoresComponent
      with FileSystemComponent
      with BagProcessingComponent
      with BagStoreComponent
      with BagFacadeComponent
      with DebugEnhancedLogging {

      override lazy val fileSystem: FileSystem = ???
      override lazy val bagProcessing: BagProcessing = ???
      override lazy val bagFacade: BagFacade = ???
      override lazy val bagStores: BagStores = mock[BagStores]
    }
  }
}
