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

import scala.xml.transform.RewriteRule
import scala.xml.{ Elem, Node, Text }

case class RceRewriteRule(cfgDir: File) extends RewriteRule {

  case class ReportCfg(uuid: String, label: String, regexp: String)

  // just one that does not match easy-dataset:99840 "Arcadis Archeologische Rapporten [2017 - 116]"
  private val an = "[-_/.a-z0-9]"
  private val digit = "[0-9]"
  val nrRegExp = s"\\W*($an+$digit$an*|$digit)"

  private val reportFile: File = cfgDir / "RCE-reports.csv"
  val reportMap: Seq[ReportCfg] = parseCsv(reportFile, 0)
    .map(r => ReportCfg(
      r.get(0),
      r.get(1),
      r.get(2).trim + nrRegExp + "([.]|:.*)?",
    )).toSeq

  override def transform(n: Node): Seq[Node] = n match {
    case Elem(_, "profile", _, _, _) => n // TODO title inside profile should not be processed
    case Elem(_, "title", _, _, Text(titleValue)) =>
      // TODO logging
      reportMap
        .filter(cfg => titleValue.trim.toLowerCase.matches(cfg.regexp))
        .map(cfg => toRceReport(titleValue, cfg.uuid))
        .headOption.getOrElse(n) // TODO warn and skip if multiple matches?
    case _ => n
  }

  private def toRceReport(titleValue: String, uuid: String): Elem = {
    <reportNumber
      schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e"
      valueURI={ s"https://data.cultureelerfgoed.nl/term/id/abr/$uuid" }
      subjectScheme="RCE rapporten"
      reportNo={ titleValue.replaceAll(s".*($nrRegExp)$$", "$1").trim }
    >{ titleValue }
    </reportNumber>
  }
}
object RceRewriteRule {
}

