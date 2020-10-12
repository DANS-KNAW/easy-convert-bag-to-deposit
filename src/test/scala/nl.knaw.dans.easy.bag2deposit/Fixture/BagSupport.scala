/**
 * Copyright (C) 2020 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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
