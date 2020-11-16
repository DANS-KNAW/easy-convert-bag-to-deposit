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

import nl.knaw.dans.easy.bag2deposit.BagSource.{ BagSource, FEDORA, VAULT, submittedStateDescription }
import nl.knaw.dans.easy.bag2deposit.IdType._
import nl.knaw.dans.lib.error.TryExtensions
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.configuration.PropertiesConfiguration

import scala.util.Try
import scala.xml.{ Node, NodeSeq }

case class DepositPropertiesFactory(configuration: Configuration, idType: IdType, bagSource: BagSource) extends DebugEnhancedLogging {
  def create(bagInfo: BagInfo, ddm: Node): Try[PropertiesConfiguration] = Try {
    val ddmIds: NodeSeq = ddm \ "dcmiMetadata" \ "identifier"

    def getIdType(idType: String) = ddmIds
      .find(_.hasType(s"id-type:$idType"))
      .getOrElse(throw InvalidBagException(s"no $idType"))
      .text

    val doi = getIdType("DOI")
    val urn = getIdType("URN")
    val fedoraId = ddmIds
      .find(_.text.startsWith("easy-dataset"))
      .getOrElse(throw InvalidBagException("no fedoraID"))
      .text

    def nbn = bagInfo.versionOf.map(
      configuration.bagIndex.getURN(_).unsafeGetOrThrow
    ).getOrElse(urn)

    def checkSequence(): Unit = {
      val seqLength = configuration.bagIndex
        .getSeqLength(bagInfo.uuid)
        .getOrRecover {
          case InvalidBagException(_) => 0 // not found
          case t => throw t
        }
      if (seqLength > 1)
        logger.warn(s"${ bagInfo.uuid } is part of a sequence")
    }

    new PropertiesConfiguration() {
      addProperty("state.label", "SUBMITTED")
      addProperty("state.description", submittedStateDescription(bagSource))
      addProperty("deposit.origin", bagSource.toString)
      addProperty("creation.timestamp", bagInfo.created)
      addProperty("depositor.userId", bagInfo.userId)
      addProperty("identifier.doi", doi)
      addProperty("identifier.urn", urn)
      addProperty("identifier.fedora", fedoraId)
      bagSource match {
        case VAULT =>
          checkSequence()
          addProperty("bag-store.bag-name", bagInfo.bagName)
          addProperty("bag-store.bag-id", bagInfo.uuid)
          addProperty("dataverse.sword-token", bagInfo.versionOf.getOrElse(bagInfo.uuid))
          addProperty("dataverse.bag-id", "urn:uuid:" + bagInfo.uuid)
          addProperty("dataverse.nbn", nbn)
        case FEDORA =>
          addProperty("dataverse.nbn", bagInfo.baseUrn.getOrElse(urn))
        case _ =>
      }
      if (!configuration.dansDoiPrefixes.contains(doi.replaceAll("/.*", "/")))
        addProperty("dataverse.other-id", "https://doi.org/" + doi)
      addProperty("dataverse.id-protocol", idType.toString.toLowerCase)
      idType match {
        case DOI =>
          addProperty("dataverse.id-identifier", doi.replaceAll(".*/", ""))
          addProperty("dataverse.id-authority", configuration.dataverseIdAutority)
        case URN =>
          addProperty("dataverse.id-identifier", urn)
          addProperty("dataverse.id-authority", "nbn:nl:ui:13")
      }
    }
  }
}
