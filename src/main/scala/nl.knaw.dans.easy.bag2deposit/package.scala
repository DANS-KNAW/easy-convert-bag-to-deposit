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

  private val encodingMap = Map[String,String](
    "<c2><a1>" -> "¡",
    "<c2><a2>" -> "¢",
    "<c2><a3>" -> "£",
    "<c2><a4>" -> "¤",
    "<c2><a5>" -> "¥",
    "<c2><a6>" -> "¦",
    "<c2><a7>" -> "§",
    "<c2><a8>" -> "¨",
    "<c2><a9>" -> "©",
    "<c2><aa>" -> "ª",
    "<c2><ab>" -> "«",
    "<c2><ac>" -> "¬",
    "<c2><ad>" -> "­",
    "<c2><ae>" -> "®",
    "<c2><af>" -> "¯",
    "<c2><b0>" -> "°",
    "<c2><b1>" -> "±",
    "<c2><b2>" -> "²",
    "<c2><b3>" -> "³",
    "<c2><b4>" -> "´",
    "<c2><b5>" -> "µ",
    "<c2><b6>" -> "¶",
    "<c2><b7>" -> "·",
    "<c2><b8>" -> "¸",
    "<c2><b9>" -> "¹",
    "<c2><ba>" -> "º",
    "<c2><bb>" -> "»",
    "<c2><bc>" -> "¼",
    "<c2><bd>" -> "½",
    "<c2><be>" -> "¾",
    "<c2><bf>" -> "¿",
    "<c3><80>" -> "À",
    "<c3><81>" -> "Á",
    "<c3><82>" -> "Â",
    "<c3><83>" -> "Ã",
    "<c3><84>" -> "Ä",
    "<c3><85>" -> "Å",
    "<c3><86>" -> "Æ",
    "<c3><87>" -> "Ç",
    "<c3><88>" -> "È",
    "<c3><89>" -> "É",
    "<c3><8a>" -> "Ê",
    "<c3><8b>" -> "Ë",
    "<c3><8c>" -> "Ì",
    "<c3><8d>" -> "Í",
    "<c3><8e>" -> "Î",
    "<c3><8f>" -> "Ï",
    "<c3><90>" -> "Ð",
    "<c3><91>" -> "Ñ",
    "<c3><92>" -> "Ò",
    "<c3><93>" -> "Ó",
    "<c3><94>" -> "Ô",
    "<c3><95>" -> "Õ",
    "<c3><96>" -> "Ö",
    "<c3><97>" -> "×",
    "<c3><98>" -> "Ø",
    "<c3><99>" -> "Ù",
    "<c3><9a>" -> "Ú",
    "<c3><9b>" -> "Û",
    "<c3><9c>" -> "Ü",
    "<c3><9d>" -> "Ý",
    "<c3><9e>" -> "Þ",
    "<c3><9f>" -> "ß",
    "<c3><a0>" -> "à",
    "<c3><a1>" -> "á",
    "<c3><a2>" -> "â",
    "<c3><a3>" -> "ã",
    "<c3><a4>" -> "ä",
    "<c3><a5>" -> "å",
    "<c3><a6>" -> "æ",
    "<c3><a7>" -> "ç",
    "<c3><a8>" -> "è",
    "<c3><a9>" -> "é",
    "<c3><aa>" -> "ê",
    "<c3><ab>" -> "ë",
    "<c3><ac>" -> "ì",
    "<c3><ad>" -> "í",
    "<c3><ae>" -> "î",
    "<c3><af>" -> "ï",
    "<c3><b0>" -> "ð",
    "<c3><b1>" -> "ñ",
    "<c3><b2>" -> "ò",
    "<c3><b3>" -> "ó",
    "<c3><b4>" -> "ô",
    "<c3><b5>" -> "õ",
    "<c3><b6>" -> "ö",
    "<c3><b7>" -> "÷",
    "<c3><b8>" -> "ø",
    "<c3><b9>" -> "ù",
    "<c3><ba>" -> "ú",
    "<c3><bb>" -> "û",
    "<c3><bc>" -> "ü",
    "<c3><bd>" -> "ý",
    "<c3><be>" -> "þ",
    "<c3><bf>" -> "ÿ",
    // https://www.utf8-chartable.de/unicode-utf8-table.pl?start=8192&number=128
    "<e2><80><90>" -> "‐",
    "<e2><80><91>" -> "‑",
    "<e2><80><92>" -> "‒",
    "<e2><80><93>" -> "–",
    "<e2><80><94>" -> "—",
    "<e2><80><95>" -> "―",
    "<e2><80><96>" -> "‖",
    "<e2><80><97>" -> "‗",
    "<e2><80><98>" -> "‘",
    "<e2><80><99>" -> "’",
    "<e2><80><9a>" -> "‚",
    "<e2><80><9b>" -> "‛",
    "<e2><80><9c>" -> "“",
    "<e2><80><9d>" -> "”",
    "<e2><80><9e>" -> "„",
    "<e2><80><9f>" -> "‟",
    "<e2><80><a0>" -> "†",
    "<e2><80><a1>" -> "‡",
    "<e2><80><a2>" -> "•",
    "<e2><80><a3>" -> "‣",
    "<e2><80><a4>" -> "․",
    "<e2><80><a5>" -> "‥",
    "<e2><80><a6>" -> "…",
    "<e2><80><a7>" -> "‧",
    "<e2><80><b0>" -> "‰",
    "<e2><80><b1>" -> "‱",
    "<e2><80><b2>" -> "′",
    "<e2><80><b3>" -> "″",
    "<e2><80><b4>" -> "‴",
    "<e2><80><b5>" -> "‵",
    "<e2><80><b6>" -> "‶",
    "<e2><80><b7>" -> "‷",
    "<e2><80><b8>" -> "‸",
    "<e2><80><b9>" -> "‹",
    "<e2><80><ba>" -> "›",
    "<e2><80><bb>" -> "※",
    "<e2><80><bc>" -> "‼",
    "<e2><80><bd>" -> "‽",
    "<e2><80><be>" -> "‾",
    "<e2><80><bf>" -> "‿",
    "<e2><81><80>" -> "⁀",
    "<e2><81><81>" -> "⁁",
    "<e2><81><82>" -> "⁂",
    "<e2><81><83>" -> "⁃",
    "<e2><81><84>" -> "⁄",
    "<e2><81><85>" -> "⁅",
    "<e2><81><86>" -> "⁆",
    "<e2><81><87>" -> "⁇",
    "<e2><81><88>" -> "⁈",
    "<e2><81><89>" -> "⁉",
    "<e2><81><8a>" -> "⁊",
    "<e2><81><8b>" -> "⁋",
    "<e2><81><8c>" -> "⁌",
    "<e2><81><8d>" -> "⁍",
    "<e2><81><8e>" -> "⁎",
    "<e2><81><8f>" -> "⁏",
    "<e2><81><90>" -> "⁐",
    "<e2><81><91>" -> "⁑",
    "<e2><81><92>" -> "⁒",
    "<e2><81><93>" -> "⁓",
    "<e2><81><94>" -> "⁔",
    "<e2><81><95>" -> "⁕",
    "<e2><81><96>" -> "⁖",
    "<e2><81><97>" -> "⁗",
    "<e2><81><98>" -> "⁘",
    "<e2><81><99>" -> "⁙",
    "<e2><81><9a>" -> "⁚",
    "<e2><81><9b>" -> "⁛",
    "<e2><81><9c>" -> "⁜",
    "<e2><81><9d>" -> "⁝",
    "<e2><81><9e>" -> "⁞",
    "<e2><81><b0>" -> "⁰",
    "<e2><81><b1>" -> "ⁱ",
    "<e2><81><b4>" -> "⁴",
    "<e2><81><b5>" -> "⁵",
    "<e2><81><b6>" -> "⁶",
    "<e2><81><b7>" -> "⁷",
    "<e2><81><b8>" -> "⁸",
    "<e2><81><b9>" -> "⁹",
    "<e2><81><ba>" -> "⁺",
    "<e2><81><bb>" -> "⁻",
    "<e2><81><bc>" -> "⁼",
    "<e2><81><bd>" -> "⁽",
    "<e2><81><be>" -> "⁾",
    "<e2><81><bf>" -> "ⁿ",
  ).map(kv => kv._1.toUpperCase -> kv._2)
  def loadXml(file: File): Try[Elem] = {
    trace(file)
    Try {
      val withoutPrologue = file.contentAsString.replaceAll("<[?].+[?]>", "")
      val string = // make sure to have a prologue
        """<?xml version="1.0" encoding="UTF-8" ?>
          |""".stripMargin + encodingMap.foldLeft(withoutPrologue)((acc, kv) => acc.replaceAll(kv._1, kv._2))
      XML.loadString(string)
    }.recoverWith {
      case _: FileNotFoundException  => Failure(InvalidBagException(s"Could not find: $file"))
      case _: NoSuchFileException => Failure(InvalidBagException(s"Could not find: $file"))
        case t: SAXParseException => Failure(InvalidBagException(s"Could not load: $file - ${ t.getMessage }"))
      }
  }

  implicit class XmlExtensions(val elem: Node) extends AnyVal {

    def serialize: String = {
      """<?xml version="1.0" encoding="UTF-8"?>
        |""".stripMargin + Utility.serialize(elem)
    }
  }
}
