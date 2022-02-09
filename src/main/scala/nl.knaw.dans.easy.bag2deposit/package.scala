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
package nl.knaw.dans.easy

import better.files.File
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.csv.{CSVFormat, CSVParser, CSVRecord}
import org.joda.time.format.{DateTimeFormatter, ISODateTimeFormat}
import org.joda.time.{DateTime, DateTimeZone}
import resource.managed

import java.io.FileNotFoundException
import java.nio.charset.Charset
import java.nio.file.NoSuchFileException
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.util.{Failure, Try}
import scala.xml._

package object bag2deposit extends DebugEnhancedLogging {

  val dateTimeFormatter: DateTimeFormatter = ISODateTimeFormat.dateTime()

  def now: String = DateTime.now(DateTimeZone.UTC).toString(dateTimeFormatter)

  case class InvalidBagException(msg: String) extends Exception(msg)

  private val xsiURI = "http://www.w3.org/2001/XMLSchema-instance"

  def parseCsv(file: File, nrOfHeaderLines: Int, format: CSVFormat = CSVFormat.RFC4180): Iterable[CSVRecord] = {
    trace(file)
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

  def loadXml(file: File): Try[(Elem, Seq[String], Seq[String])] = {

    val oldChars = new ListBuffer[String]()
    val newChars = new ListBuffer[String]()
    def convert(matchValue: Regex.Match): String = matchValue match {
      case _ =>
        val bytes = matchValue.toString.replaceAll("[<>]", "").grouped(2).toList.map(s =>
          Integer.parseInt(s, 16).toByte
        ).toArray
        val str = new String(bytes, Charset.forName("UTF-8"))
        oldChars.append(matchValue.toString())
        newChars.append(str)
        str
    }
    trace(file)
    // covering <a0> through <ff> as first unicode byte
    // https://www.unicode.org/charts/PDF/ does not link to existing
    // https://www.unicode.org/charts/PDF/UA500.pdf https://www.unicode.org/charts/PDF/UA700.pdf
    val regexp = {
      val d = "[0-9a-fA-F]"
      val dd = s"<$d{2}>"
      val b2 = s"<[a-dA-D]$d>$dd"
      val b3 = s"<[eE]$d>$dd$dd"
      val b4 = s"<[fF]$d>$dd$dd$dd"
      s"(?s)(($b2)|($b3)|($b4))".r
    }
    Try {
      val withoutPrologue = file
        .contentAsString(Charset.forName("UTF-8"))
        .replaceAll("<[?].+[?]>", "")
      val s = regexp.replaceAllIn(withoutPrologue, matchValue => convert(matchValue))
      val xml = XML.loadString(
        """<?xml version="1.0" encoding="UTF-8" ?>
          |""".stripMargin + s
      )
      (xml, oldChars, newChars)
    }.recoverWith {
      case _: FileNotFoundException => Failure(InvalidBagException(s"Could not find: $file"))
      case _: NoSuchFileException => Failure(InvalidBagException(s"Could not find: $file"))
      case t: SAXParseException => Failure(InvalidBagException(s"Could not load: $file - ${t.getMessage}"))
    }
  }

  implicit class XmlExtensions(val elem: Node) extends AnyVal {

    def serialize: String = {
      """<?xml version="1.0" encoding="UTF-8"?>
        |""".stripMargin + Utility.serialize(elem)
    }
  }
}
