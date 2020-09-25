package nl.knaw.dans.easy.v2ip

import better.files.File
import nl.knaw.dans.easy.bagstore.component.{ BagStoreWiring, BagStoresComponent }
import nl.knaw.dans.easy.bagstore.{ BagFacadeComponent, ConfigurationComponent }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

object BagStoresWiring {
  def apply(home: File): BagStoresComponent = new BagStoreWiring
    with ConfigurationComponent
    with BagFacadeComponent
    with DebugEnhancedLogging {
    override lazy val bagFacade: BagFacade = new BagFacade {}
    override lazy val configuration: Configuration = Configuration(home.path)
  }
}
