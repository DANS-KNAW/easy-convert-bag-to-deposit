/*
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

import better.files.File
import gov.loc.repository.bagit.domain.Metadata
import nl.knaw.dans.lib.error._
import org.apache.commons.configuration.ConfigurationException

import java.util.UUID
import scala.collection.JavaConverters._
import scala.util.{ Failure, Try }

case class BagInfo(userId: String,
                   created: String,
                   uuid: UUID,
                   bagName: String,
                   versionOf: Option[UUID],
                   bagSeqNr: Int  = 1,
                   basePids: Option[BasePids] = None,
                  )

object BagInfo {
  // these values should match easy-fedora-to-bag
  val baseUrnKey = "Base-Urn"
  val baseDoiKey = "Base-DOI"

  def apply(bagDir: File, bagInfo: Metadata): Try[BagInfo] = Try {
    def getMaybe(key: String) = Option(bagInfo.get(key))
      .flatMap(_.asScala.headOption)

    def notFound(key: String) = InvalidBagException(s"No $key in $bagDir/bag-info.txt")

    def getMandatory(key: String) = getMaybe(key).getOrElse(throw notFound(key))

    val maybeVersionOf = getMaybe(BagFacade.IS_VERSION_OF_KEY).map(uuidFromVersionOf)
    val basePids = (getMaybe(baseUrnKey), getMaybe(baseDoiKey)) match {
      case (None, None) => None
      case (Some(urn), Some(doi)) => Some(BasePids(urn, doi))
      case _ => throw new Exception("")
    }

    // workaround when no seqNr was provided by easy-fedora-to-bag (works if a dataset produced at most two bags)
    val defaultSeqNr = maybeVersionOf.map(_ => "2").getOrElse("1")
    val maybeSeqNr = Option(bagInfo.get(BagFacade.BAG_SEQUENCE_NUMBER)).flatMap(_.asScala.headOption)
    new BagInfo(
      userId = getMandatory(BagFacade.EASY_USER_ACCOUNT_KEY),
      created = getMandatory("Bagging-Date"),
      uuid = uuidFromFile(bagDir.parent),
      bagName = bagDir.name,
      versionOf = maybeVersionOf,
      basePids = basePids,
      bagSeqNr = maybeSeqNr.getOrElse(defaultSeqNr).toInt,
    )
  }.recoverWith { case e: ConfigurationException =>
    Failure(InvalidBagException(e.getMessage))
  }

  private def uuidFromVersionOf(s: String): UUID = Try {
    UUID.fromString(s.replaceAll("urn:uuid:", ""))
  }.recoverWith {
    case _ => throw InvalidBagException(s"Invalid UUID: Is-Version-Of: $s")
  }.unsafeGetOrThrow

  private def uuidFromFile(f: File): UUID = Try {
    UUID.fromString(f.name)
  }.recoverWith {
    case _ => throw InvalidBagException(s"Invalid UUID: $f")
  }.unsafeGetOrThrow
}
