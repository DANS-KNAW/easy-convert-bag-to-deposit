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

  case class ReportCfg(uuid: String, label: String, regexpWithNr: String, regexp: String)

  val reportMap: Seq[ReportCfg] = parseCsv(cfgDir / "ABR-reports.csv", 0)
    .map(r => ReportCfg(
      uuid = r.get(0),
      label = r.get(1),
      regexpWithNr = r.get(2).trim + nrTailRegexp,
      regexp = r.get(2).trim,
    )).toSeq

  val nodeLabels = Seq("title", "alternative", "identifier")

  override def transform(node: Node): Seq[Node] = {
    lazy val value = node.text
    lazy val lowerCaseValue = value.trim.toLowerCase

    def logIfMissed(reports: Seq[Elem]) = {
      if (reports.isEmpty && lowerCaseValue.matches(missedRegExp))
        logger.info(s"potential report number: $value")
      reports
    }

    val reports = node.label match {
      case "title" | "alternative" =>
        logIfMissed(transformTitle(value, lowerCaseValue))
      case _ if value.matches(s"$nrRegexp *[(].*") =>
        logIfMissed(transformIdWithBrackets(value, lowerCaseValue))
      case _ if value.contains(" ") =>
        logIfMissed(transformId(value, lowerCaseValue))
      case _ => node
    }
    if (value != reports.text)
      reports :+ node
    else reports
  }

  private def transformId(value: String, lowerCaseValue: String) = {
    mapToReport(
      nr = value.replaceAll(s".*( +:)? +($nrRegexp)", "$2").trim,
      originalNameWithNr = value,
      lowerCaseName = lowerCaseValue
        .replaceAll(s"( +:)? +$nrRegexp", "")
    )
  }

  private def transformIdWithBrackets(originalContent: String, lowerCaseContent: String) = {
    mapToReport(
      nr = originalContent.replaceAll(s" *[(].*", "").trim,
      originalNameWithNr = originalContent,
      lowerCaseName = lowerCaseContent
        .replaceAll(".*[(]", "")
        .replaceAll("[)].*", "")
    )
  }

  private def transformTitle(originalContent: String, lowerCaseValue: String) = {
    mapToReport(
      nr = originalContent.replaceAll(s".* ($nrRegexp)$trailer", "$1").trim,
      originalNameWithNr = originalContent.replaceAll(trailer + "$", ""),
      lowerCaseName = lowerCaseValue.replaceAll(s" +$nrRegexp$trailer$$", "")
    )
  }

  private def mapToReport(nr: String, originalNameWithNr: String, lowerCaseName: String) = {
    reportMap
      .filter(cfg => lowerCaseName.matches(cfg.regexp))
      .map(cfg => toReportNr(originalNameWithNr, nr, cfg.uuid))
  }

  private def toReportNr(originalNameWithNr: String, nr: String, uuid: String): Elem = {
    <ddm:reportNumber
      schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e"
      valueURI={ s"https://data.cultureelerfgoed.nl/term/id/abr/$uuid" }
      subjectScheme="ABR Rapporten"
      reportNo={ nr }
    >{ originalNameWithNr }</ddm:reportNumber>
  }
}
object ReportRewriteRule extends DebugEnhancedLogging {

  val digit = "[0-9]"

  /** alpha numeric (and a little more) */
  val an = "[-_/.a-zA-Z0-9]"

  /** just one that does not match easy-dataset:99840 "Arcadis Archeologische Rapporten [2017 - 116]" */
  val nrRegexp = s"$an*$digit$an*"

  private val trailer = "([.]|:.*)?"
  private val nrTailRegexp = s" +$nrRegexp$trailer"
  private val missedRegExp = s".*(notitie|rapport|bericht|publicat).* +$nrRegexp$trailer"

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
