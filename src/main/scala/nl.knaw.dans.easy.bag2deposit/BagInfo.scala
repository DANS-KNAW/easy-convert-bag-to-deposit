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

import better.files.File
import gov.loc.repository.bagit.domain.Bag
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.lib.error._
import org.apache.commons.configuration.ConfigurationException

import scala.collection.JavaConverters._
import scala.util.{ Failure, Try }

case class BagInfo(userId: String, created: String, uuid: UUID, bagName: String, versionOf: Option[UUID], baseUrn: Option[String] = None)

object BagInfo {
  val baseUrnKey = "Base-Urn"

  def apply(bag: Bag, requireBaseUrnWithVersionOf: Boolean): Try[BagInfo] = Try {
    val bagDir = File(bag.getRootDir)
    val bagInfo = bag.getMetadata

    def getMaybe(key: String) = Option(bagInfo.get(key))
      .flatMap(_.asScala.headOption)

    def notFound(key: String) = InvalidBagException(s"No $key in $bagDir/bag-info.txt")

    def getMandatory(key: String) = getMaybe(key).getOrElse(throw notFound(key))

    val maybeVersionOf = getMaybe(DansV0Bag.IS_VERSION_OF_KEY).map(uuidFromVersionOf)
    val maybeBaseUrn = getMaybe(baseUrnKey)

    if (maybeVersionOf.isDefined && requireBaseUrnWithVersionOf && maybeBaseUrn.isEmpty)
      throw notFound(baseUrnKey)

    new BagInfo(userId = getMandatory(DansV0Bag.EASY_USER_ACCOUNT_KEY), created = getMandatory("Bagging-Date"), uuid = uuidFromFile(bagDir.parent), bagName = bagDir.name, versionOf = maybeVersionOf, baseUrn = maybeBaseUrn)
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
