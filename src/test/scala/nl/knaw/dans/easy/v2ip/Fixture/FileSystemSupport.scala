package nl.knaw.dans.easy.v2ip.Fixture

import better.files.File
import better.files.File.currentWorkingDirectory
import org.scalatest.BeforeAndAfterEach
import org.scalatest.enablers.Existence
import org.scalatest.flatspec.AnyFlatSpec

trait FileSystemSupport extends BeforeAndAfterEach {
  this: AnyFlatSpec =>

  implicit def existenceOfFile[FILE <: better.files.File]: Existence[FILE] = _.exists

  override def beforeEach(): Unit = {
    super.beforeEach()

    if (testDir.exists) testDir.delete()
    testDir.createDirectories()
  }

  lazy val testDir: File = currentWorkingDirectory / "target" / "test" / getClass.getSimpleName
}
