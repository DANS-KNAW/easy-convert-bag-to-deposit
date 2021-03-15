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
import nl.knaw.dans.easy.bag2deposit.parseCsv
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.csv.CSVFormat

import scala.util.matching.Regex
import scala.xml.transform.RewriteRule
import scala.xml.{ Elem, Node }

case class AbrRewriteRule(oldLabel: String, map: Map[String, Elem]) extends RewriteRule with DebugEnhancedLogging {
  override def transform(node: Node): Seq[Node] = {
    if (!isAbr(node)) node
    else {
      val value = node.text
      // extract last expression between brackets
      val key = value.trim.replaceAll(".*[(]","").replace(")","").trim
      if (key.isEmpty)
        <notImplemented>{ value }</notImplemented>
      else map.getOrElse(key, <notImplemented>{ s"$oldLabel $key not found" }</notImplemented>)
    }
  }

  private def isAbr(node: Node) = {
    val attr = node.attributes
    node.label == oldLabel &&
      (
        (attr.prefixedKey == "xsi:type" && attr.value.mkString("").startsWith("abr:ABR")) ||
          (attr.get("valueURI").mkString("").startsWith("http://www.rnaproject.org/"))
        )
  }
}

object AbrRewriteRule {
  val nrOfHeaderLines = 2
  private val csvFormat = CSVFormat.RFC4180
    .withHeader("old", "new", "uuid", "description")
    .withDelimiter(',')
    .withRecordSeparator('\n')

  def temporalRewriteRule(cfgDir: File): AbrRewriteRule = {
    new AbrRewriteRule("temporal", temporalMap(cfgDir))
  }

  def subjectRewriteRule(cfgDir: File): AbrRewriteRule = {
    new AbrRewriteRule("subject", subjectMap(cfgDir))
  }

  private def valueUri(uuid: String) = s"https://data.cultureelerfgoed.nl/term/id/abr/$uuid"

  private def temporalMap(cfgDir: File): Map[String, Elem] = {
    parseCsv(cfgDir / "ABR-period.csv", nrOfHeaderLines, csvFormat)
      .map(record => record.get("old") ->
        <ddm:temporal xml:lang="nl"
           valueURI={ valueUri(record.get("uuid")) }
           subjectScheme={ "ABR Periodes" }
           schemeURI={ "https://data.cultureelerfgoed.nl/term/id/abr/9b688754-1315-484b-9c89-8817e87c1e84" }
        >{ record.get("description") }</ddm:temporal>
      ).toMap
  }

  private def subjectMap(cfgDir: File): Map[String, Elem] = {
    parseCsv(cfgDir / "ABR-complex.csv", nrOfHeaderLines, csvFormat)
      .map(record => record.get("old") ->
        <ddm:subject xml:lang="nl"
          valueURI={ valueUri(record.get("uuid")) }
          subjectScheme={ "ABR Complextypen" }
          schemeURI={ "https://data.cultureelerfgoed.nl/term/id/abr/e9546020-4b28-4819-b0c2-29e7c864c5c0" }
        >{ record.get("description") }</ddm:subject>
      ).toMap
  }
}
