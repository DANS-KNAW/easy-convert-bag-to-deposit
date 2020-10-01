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
package nl.knaw.dans.easy.bag2deposit

import java.util.UUID

import nl.knaw.dans.easy.bag2deposit.IdType._
import nl.knaw.dans.lib.error.TryExtensions
import org.apache.commons.configuration.PropertiesConfiguration

import scala.util.Try
import scala.xml.{ Elem, NodeSeq }

case class DepositPropertiesFactory(configuration: Configuration) {
  def create(bagInfo: BagInfo, ddm: Elem, idType: IdType): Try[PropertiesConfiguration] = Try {
    val ddmIds: NodeSeq = ddm \ "dcmiMetadata" \ "identifier"

    def getBaseUrn(versionOf: UUID) = configuration.bagIndex.get(versionOf).map(_.urn).unsafeGetOrThrow

    def getIdType(idType: String) = ddmIds
      .find(_.hasType(s"id-type:$idType"))
      .getOrElse(throw InvalidBagException(s"no $idType"))
      .text

    val fedoraId = ddmIds
      .find(_.text.startsWith("easy-dataset"))
      .getOrElse(throw InvalidBagException("no fedoraID"))
      .text

    val doi = getIdType("DOI")
    val urn = getIdType("URN")

    new PropertiesConfiguration() {
      addProperty("state.label", "SUBMITTED")
      addProperty("state.description", "This deposit was extracted from the vault and is ready for processing")
      addProperty("deposit.origin", "vault")
      // TODO when needed: make constant values above configurable for other origins.
      addProperty("creation.timestamp", bagInfo.created)
      addProperty("depositor.userId", bagInfo.userId)
      addProperty("bag-store.bag-id", bagInfo.uuid)
      addProperty("bag-store.bag-name", bagInfo.bagName)
      addProperty("identifier.doi", doi)
      addProperty("identifier.urn", urn)
      addProperty("identifier.fedora", fedoraId)
      addProperty("dataverse.bag-id", "urn:uuid:" + bagInfo.uuid)
      addProperty("dataverse.sword-token", "urn:uuid:" + bagInfo.versionOf.getOrElse(bagInfo.uuid))
      addProperty("dataverse.nbn", "urn:uuid:" + bagInfo.versionOf.map(getBaseUrn).getOrElse(urn))
      if (!configuration.dansDoiPrefixes.contains(doi.replaceAll("/.*", "/")))
        addProperty("dataverse.other-id", doi)
      idType match {
        case DOI => addProperty("dataverse.identifier", doi)
        case URN => addProperty("dataverse.identifier", urn)
      }
    }
  }
}
