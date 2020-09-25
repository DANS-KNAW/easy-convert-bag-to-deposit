package nl.knaw.dans.easy.v2ip

import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.v2ip.Fixture.FileSystemSupport
import org.scalatest.flatspec.AnyFlatSpec

class AppSpec extends AnyFlatSpec with FileSystemSupport {
  "createSips" should "" in {
    File("src/test/resources/bags").children.toSeq.foreach(_.copyTo((testDir / "vault" / UUID.randomUUID().toString).createDirectories()))
    val uuids = (testDir / "vault" / UUID.randomUUID().toString).createDirectories().children.map(f =>UUID.fromString(f.name))
    new EasyVaultExportIpApp(new Configuration("testVersion",testDir/"staging", null))
      .createSips(uuids, IdType.DOI,testDir / "output", testDir / "log.csv")
  }
}
