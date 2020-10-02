package nl.knaw.dans.easy.bag2deposit.Fixture

import nl.knaw.dans.easy.bag2deposit.{ BagIndex, Configuration }
import org.scalamock.scalatest.MockFactory

trait AppConfigSupport extends MockFactory{
  def mockedConfig: Configuration = {
    new Configuration(
      version = "testVersion",
      dansDoiPrefixes = Seq("10.17026/", "10.5072/"),
      bagIndex = mock[BagIndex],
    )
  }
}
