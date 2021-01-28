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
import nl.knaw.dans.easy.bag2deposit.ddm.ReportRewriteRule.{ missedRegExp, nrRegexp, nrTailRegexp, trailer }
import nl.knaw.dans.easy.bag2deposit.parseCsv
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.xml.transform.RewriteRule
import scala.xml.{ Elem, Node, NodeSeq }

case class ReportRewriteRule(cfgDir: File) extends RewriteRule with DebugEnhancedLogging {

  case class ReportCfg(uuid: String, label: String, regexp: String)

  val reportMap: Seq[ReportCfg] = parseCsv(cfgDir / "ABR-reports.csv", 0)
    .map(r => ReportCfg(
      uuid = r.get(0),
      label = r.get(1),
      regexp = r.get(2).trim + nrTailRegexp,
    )).toSeq

  override def transform(n: Node): Seq[Node] = {
    if (n.label != "title" && n.label != "alternative") n
    else {
      val titleValue = n.text
      val lowerCaseTitle = titleValue.trim.toLowerCase
      val reports = reportMap
        .filter(cfg => lowerCaseTitle.matches(cfg.regexp))
        .map(cfg => toReportNr(titleValue.replaceAll(":.*", ""), cfg.uuid))
        .theSeq
      if (reports.isEmpty && lowerCaseTitle.matches(missedRegExp))
        logger.info(s"potential report number: $titleValue")
      if (titleValue == reports.text)
        reports
      else reports :+ n
    }
  }

  private def toReportNr(titleValue: String, uuid: String): Elem = {
    <ddm:reportNumber
      schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e"
      valueURI={ s"https://data.cultureelerfgoed.nl/term/id/abr/$uuid" }
      subjectScheme="ABR Rapporten"
      reportNo={ titleValue.replaceAll(s".*($nrRegexp)$trailer", "$1").trim }
    >{ titleValue }</ddm:reportNumber>
  }
}
object ReportRewriteRule extends DebugEnhancedLogging {

  private val digit = "[0-9]"

  /** alpha numeric (and a little more) */
  private val an = "[-_/.a-z0-9]"

  /** just one that does not match easy-dataset:99840 "Arcadis Archeologische Rapporten [2017 - 116]" */
  val nrRegexp = s"\\W+$an*$digit$an*"

  private val trailer = "([.]|:.*)?"
  private val nrTailRegexp = s"$nrRegexp$trailer"
  private val missedRegExp = s".*(notitie|rapport|bericht|publicat).*$nrRegexp$trailer"

  def logBriefRapportTitles(notConvertedTitles: NodeSeq, ddmOut: Node, datasetId: String): Unit = {
    // note: some of notConverted may have produced a reportNumber, the ones logged below won't
    // a ReportRewriteRule instance knows that difference but has no datasetId/rightsHolder/publisher for its logging

    // these titles need a more complex transformation or manual fix before the final export
    notConvertedTitles.foreach { node =>
      val title = node.text
      if (title.toLowerCase.matches(s"brief[^a-z]*rapport$nrTailRegexp }"))
        logger.info(s"$datasetId - briefrapport rightsHolder=[${ ddmOut \ "rightsHolder" }] publisher=[${ ddmOut \ "publisher" }] titles=[$title]")
    }
  }
}
