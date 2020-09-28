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

import better.files.File
import org.apache.commons.configuration.PropertiesConfiguration

import scala.util.Try

case class BagInfo(userId: String, versionOf: Option[String])

object BagInfo {
  def apply(bagInfo: File): Try[BagInfo] = Try {
    val properties = new PropertiesConfiguration() {
      setDelimiterParsingDisabled(true)
      load(bagInfo.toJava)
    }
    BagInfo(
      properties.getString("EASY-User-Account"),
      Option(properties.getString("Is-Version-Of", null)),
    )
  }
}
