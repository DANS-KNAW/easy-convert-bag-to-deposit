package nl.knaw.dans.easy.bag2deposit

import better.files.File
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConfigurationSpec extends AnyFlatSpec with Matchers {
  "dist" should "succeed" in {
    Configuration(File("src/main/assembly/dist")) shouldBe a[Configuration]
  }
}
