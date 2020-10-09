package nl.knaw.dans.easy.bag2deposit.Fixture

import better.files.File
import gov.loc.repository.bagit.domain.{ Bag, Metadata }
import org.apache.commons.configuration.PropertiesConfiguration

trait BagSupport {
  def loadBag(file: File): Bag = {
    val props = new PropertiesConfiguration() {
      setDelimiterParsingDisabled(true)
      load((file / "bag-info.txt").toJava)
    }

    val bag = new Bag()
    bag.setRootDir(file.path)
    bag.setMetadata(new Metadata() {
      props.getKeys.forEachRemaining(key =>
        add(key, props.getString(key))
      )
    })
    bag
  }
}
