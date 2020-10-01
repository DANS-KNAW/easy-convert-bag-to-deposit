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
package nl.knaw.dans.easy

import org.joda.time.format.{ DateTimeFormatter, ISODateTimeFormat }
import org.joda.time.{ DateTime, DateTimeZone }

import scala.xml.{ Node, PrettyPrinter, Utility }

package object bag2deposit {

  val dateTimeFormatter: DateTimeFormatter = ISODateTimeFormat.dateTime()

  def now: String = DateTime.now(DateTimeZone.UTC).toString(dateTimeFormatter)

  case class InvalidBagException(msg: String) extends Exception(msg)

  private val xsiURI = "http://www.w3.org/2001/XMLSchema-instance"

  implicit class RichNode(val left: Node) extends AnyVal {

    def hasType(t: String): Boolean = {
      left.attribute(xsiURI, "type")
        .map(_.text)
        .contains(t)
    }
  }
  private val logPrinter = new PrettyPrinter(-1, 0)
  val printer = new PrettyPrinter(160, 2)

  implicit class XmlExtensions(val elem: Node) extends AnyVal {

    def toOneLiner: String = {
      logPrinter.format(Utility.trim(elem)).trim
    }
  }
}
