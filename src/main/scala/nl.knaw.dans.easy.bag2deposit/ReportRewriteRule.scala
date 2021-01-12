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

case class ReportRewriteRule(cfgDir: File) extends RewriteRule {

  case class ReportCfg(uuid: String, label: String, regexp: String)

  // just one that does not match easy-dataset:99840 "Arcadis Archeologische Rapporten [2017 - 116]"
  private val an = "[-_/.a-z0-9]"
  private val digit = "[0-9]"
  private val trailer = "([.]|:.*)?"
  val nrRegExp = s"\\W+$an*$digit$an*"

  val reportMap: Seq[ReportCfg] = parseCsv(cfgDir / "ABR-reports.csv", 0)
    .map(r => ReportCfg(
      r.get(0),
      r.get(1),
      r.get(2).trim + nrRegExp + trailer,
    )).toSeq

  override def transform(n: Node): Seq[Node] = n match {
    case Elem(_, "profile", _, _, _) => n
    case Elem(_, "title", _, _, Text(titleValue)) =>
      val reports = reportMap
        .filter(cfg => titleValue.trim.toLowerCase.matches(cfg.regexp))
        .map(cfg => toReportNr(titleValue, cfg.uuid))
        .theSeq
      if (reports.isEmpty) n
      else if (titleValue.matches(s".*$nrRegExp:.+")) reports :+ n
           else reports
    case _ => n
  }

  private def toReportNr(titleValue: String, uuid: String): Elem = {
    <ddm:reportNumber
      schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e"
      valueURI={ s"https://data.cultureelerfgoed.nl/term/id/abr/$uuid" }
      subjectScheme="ABR Rapporten"
      reportNo={ titleValue.replaceAll(s".*($nrRegExp)$trailer", "$1").trim }
    >{ titleValue }</ddm:reportNumber>
  }
}
