package nl.knaw.dans.easy.bag2deposit.Fixture

import java.util.UUID

import nl.knaw.dans.easy.bag2deposit.{ BagIndex, BagIndexInfo, Configuration }
import org.scalamock.scalatest.MockFactory

import scala.util.Success

trait AppConfigSupport extends MockFactory {
  def mockedConfig(bagIndexInfo: BagIndexInfo*): Configuration = {
    val config = new Configuration(
      version = "testVersion",
      dansDoiPrefixes = Seq("10.17026/", "10.5072/"),
      bagIndex = mock[BagIndex],
    )
    bagIndexInfo
      .foreach(info =>
        (config.bagIndex.get(_: UUID)) expects info.baseId returning Success(info)
      )
    config
  }
}
