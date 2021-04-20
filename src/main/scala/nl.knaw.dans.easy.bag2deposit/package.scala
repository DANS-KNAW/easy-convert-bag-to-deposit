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

import better.files.File
import nl.knaw.dans.lib.error._
import org.apache.commons.csv.{ CSVFormat, CSVParser, CSVRecord }
import org.joda.time.format.{ DateTimeFormatter, ISODateTimeFormat }
import org.joda.time.{ DateTime, DateTimeZone }
import resource.managed

import java.io.FileNotFoundException
import java.nio.charset.Charset
import java.nio.charset.Charset.defaultCharset
import scala.collection.JavaConverters._
import scala.util.{ Failure, Try }
import scala.xml._

package object bag2deposit {

  val dateTimeFormatter: DateTimeFormatter = ISODateTimeFormat.dateTime()

  def now: String = DateTime.now(DateTimeZone.UTC).toString(dateTimeFormatter)

  case class InvalidBagException(msg: String) extends Exception(msg)

  private val xsiURI = "http://www.w3.org/2001/XMLSchema-instance"

  def parseCsv(file: File, nrOfHeaderLines: Int, format: CSVFormat = CSVFormat.RFC4180): Iterable[CSVRecord] = {
    managed(CSVParser.parse(file.toJava, Charset.forName("UTF-8"), format))
      .map(_.asScala.filter(_.asScala.nonEmpty).drop(nrOfHeaderLines))
      .tried.unsafeGetOrThrow
  }

  implicit class RichNode(val left: Node) extends AnyVal {

    def hasType(t: String): Boolean = {
      left.attribute(xsiURI, "type")
        .map(_.text)
        .contains(t)
    }
  }

  def loadXml(file: File): Try[Elem] = Try(XML.loadFile(file.toJava))
    .recoverWith {
      case t: FileNotFoundException => Failure(InvalidBagException(s"could not find: $file"))
      case t: SAXParseException => Failure(InvalidBagException(s"could not load: $file - ${ t.getMessage }"))
    }

  implicit class XmlExtensions(val elem: Node) extends AnyVal {

    def serialize: String = {
      """<?xml version="1.0" encoding="UTF-8"?>
        |""".stripMargin + Utility.serialize(elem)
    }
  }
}
