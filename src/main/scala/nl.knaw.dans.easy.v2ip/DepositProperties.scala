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
package nl.knaw.dans.easy.v2ip

import org.apache.commons.configuration.PropertiesConfiguration

import scala.util.Try
import scala.xml.Elem

class DepositProperties extends PropertiesConfiguration {
  def fill(bagInfo: BagInfo, ddm: Elem): Try[PropertiesConfiguration] = Try {
    new PropertiesConfiguration() {
      addProperty("creation.timestamp", bagInfo.created)
      addProperty("depositor.userId", bagInfo.userId)
      addProperty("bag-store.bag-id", bagInfo.uuid)
      addProperty("bag-store.bag-name", bagInfo.bagName)
      //      addProperty("identifier.doi", csvRecord.doi)
      //      addProperty("identifier.fedora", csvRecord.easyDatasetId)
    }
  }
}

object DepositProperties {

  def default(): DepositProperties = {

    new DepositProperties() {
      addProperty("state.label", "SUBMITTED")
      addProperty("state.description", "This deposit was extracted from the vault and is ready for processing")
      addProperty("deposit.origin", "vault")
    }
  }
}
