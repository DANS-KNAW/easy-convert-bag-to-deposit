package nl.knaw.dans.easy.bag2deposit.Fixture

import better.files.File
import gov.loc.repository.bagit.domain.Bag
import org.apache.commons.configuration.PropertiesConfiguration

trait BagSupport {
  /** @param bagRoot the bag-info.txt is loaded as Metadata
   *                 the bagit.txt is required to recalculate the tagmanifest
   */
  def mockBag(bagRoot: File): Bag = {
    val props = new PropertiesConfiguration() {
      setDelimiterParsingDisabled(true)
      load((bagRoot / "bag-info.txt").toJava)
    }

    val bag = new Bag()
    bag.setRootDir(bagRoot.path)
    val md = bag.getMetadata
    props.getKeys.forEachRemaining(key =>
      md.add(key, props.getString(key))
    )
    bag
  }
}
