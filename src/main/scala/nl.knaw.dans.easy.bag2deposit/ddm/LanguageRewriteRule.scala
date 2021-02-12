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
package nl.knaw.dans.easy.bag2deposit.ddm

import better.files.File
import nl.knaw.dans.easy.bag2deposit.{ parseCsv, printer }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.xml.{ Node, NodeSeq }
import scala.xml.transform.RewriteRule

case class LanguageRewriteRule(cfgFile: File) extends RewriteRule {

  private val freeLanguageToCodes = parseCsv(cfgFile, 0).toSeq
    .map(r => r.get(0) -> r.get(1).split(",").toSeq)
    .toMap

  private val codeToElem = Map(
    "nld" -> elem("Dutch")("dut"),
    "dut" -> elem("Dutch")("dut"),
    "eng" -> elem("English")("eng"),
    "fre" -> elem("French")("fre"),
    "fra" -> elem("French")("fre"),
    "deu" -> elem("German")("ger"),
    "ger" -> elem("German")("ger"),
  )

  override def transform(node: Node): Seq[Node] = {
    def mapFreeLanguageValue = {
      freeLanguageToCodes
        .get(node.text)
        .map(_.map(elem(node.text.trim)))
        .getOrElse(node)
    }

    if (node.label != "language") node
    else if (node.attributes.mkString.matches("(.*:)?ISO639-[23]."))
           codeToElem.getOrElse(node.text.trim.toLowerCase, mapFreeLanguageValue)
         else mapFreeLanguageValue
  }

  private def elem(value: String)(code: String) =
    <ddm:language encodingScheme='ISO639-2' code={ code.toLowerCase }>{ value.trim }</ddm:language>
}
object LanguageRewriteRule extends DebugEnhancedLogging {
  def logNotMappedLanguages(ddm: Node, datasetId: String): Unit = {
    (ddm \\ "language").filter(
      !_.attributes.mkString.contains("encodingScheme")
    ).foreach(n =>
      logger.info(s"NOT MAPPED $datasetId ${printer.format(n)}")
    )
  }
}
