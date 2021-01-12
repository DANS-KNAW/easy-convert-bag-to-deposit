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

import better.files.File
import nl.knaw.dans.easy.bag2deposit.AbrRewriteRule.{ find, isAbr, parse }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.csv.CSVRecord

import scala.xml.transform.{ RewriteRule, RuleTransformer }
import scala.xml.{ Elem, MetaData, Node, Text }

case class AbrRewriteRule(cfgDir: File) extends RewriteRule {

  private val periodFile: File = cfgDir / "ABR-period.csv"
  private val periodMap = parse(periodFile, "ddm:temporal")
  private val complexFile: File = cfgDir / "ABR-complex.csv"
  private val complexMap = parse(complexFile, "ddm:subject")

  // don't replace the title in <ddm:profile>
  private val dcmiMetadataTransformer = new RuleTransformer(ReportRewriteRule(cfgDir))

  override def transform(n: Node): Seq[Node] = n match {
    case Elem(_, "dcmiMetadata", _, _, _*) => dcmiMetadataTransformer(n)
    case Elem(_, "temporal", attr: MetaData, _, Text(key)) if isAbr(attr) => find(key, periodMap, periodFile)
    case Elem(_, "subject", attr: MetaData, _, Text(key)) if isAbr(attr) => find(key, complexMap, complexFile)
    case _ => n
  }
}
object AbrRewriteRule extends DebugEnhancedLogging {
  val nrOfHeaderLines = 2

  private def isAbr(attr: MetaData) = {
    attr.prefixedKey == "xsi:type" && attr.value.mkString("").startsWith("abr:ABR")
  }

  private def find(key: String, map: Map[String, Node], file: File): Node = {
    map.getOrElse(key, throw new Exception(s"$key not found in $file"))
  }

  private def parse(file: File, label: String): Map[String, Elem] = {
    parseCsv(file, nrOfHeaderLines)
      .map(toXml(label))
      .toMap
  }

  private def valueUri(uuid: String) = s"https://data.cultureelerfgoed.nl/term/id/abr/$uuid"

  private def toXml(label: String)(record: CSVRecord): (String, Elem) = {
    val xml = <label xml:lang="nl"
                           valueURI={ valueUri(record.get(2)) }
                           subjectScheme="Archeologisch Basis Register"
                           schemeURI={ "https://data.cultureelerfgoed.nl/term/id/abr/b6df7840-67bf-48bd-aa56-7ee39435d2ed" }
              >{ record.get(3) }</label>

    (record.get(0), xml.copy(label = label))
  }
}
