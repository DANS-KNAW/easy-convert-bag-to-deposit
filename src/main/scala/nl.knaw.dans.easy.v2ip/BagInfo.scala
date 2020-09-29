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

import java.util.UUID

import better.files.File
import org.apache.commons.configuration.{ ConfigurationException, PropertiesConfiguration }

import scala.util.{ Failure, Try }

case class BagInfo(userId: String, versionOf: Option[String], created: String, uuid: UUID, bagName: String)

object BagInfo {
  def apply(bagInfo: File): Try[BagInfo] = Try {
    val properties = new PropertiesConfiguration() {
      setDelimiterParsingDisabled(true)
      load(bagInfo.toJava)
    }

    def getOptional(key: String) = Option(properties.getString(key, null))

    def getMandatory(key: String) = getOptional(key)
      .getOrElse(throw InvalidBagException(s"No $key in $bagInfo"))

    BagInfo(
      getMandatory("EASY-User-Account"),
      getOptional("Is-Version-Of"),
      getMandatory("Bagging-Date"),
      UUID.fromString(bagInfo.parent.parent.name),
      bagInfo.parent.name,
    )
  }.recoverWith {
    case e: ConfigurationException =>
      Failure(InvalidBagException(e.getMessage))
    case e if e.isInstanceOf[IllegalArgumentException] =>
      Failure(InvalidBagException(e.getMessage))
  }
}
