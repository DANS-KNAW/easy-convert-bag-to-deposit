package nl.knaw.dans.easy.bag2deposit.ddm

import better.files.File
import nl.knaw.dans.easy.bag2deposit.parseCsv
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.xml.transform.RewriteRule
import scala.xml.{ Elem, Node }

case class ReportRewriteRule(cfgDir: File) extends RewriteRule with DebugEnhancedLogging {

  case class ReportCfg(uuid: String, label: String, regexp: String)

  private val digit = "[0-9]"

  /** alpha numeric (and a little more) */
  private val an = "[-_/.a-z0-9]"

  /** just one that does not match easy-dataset:99840 "Arcadis Archeologische Rapporten [2017 - 116]" */
  val nrRegexp = s"\\W+$an*$digit$an*"

  private val trailer = "([.]|:.*)?"
  val nrTailRegexp = s"$nrRegexp$trailer"
  private val missedRegExp = s".*(notitie|rapport|bericht|publicat).*$nrRegexp$trailer"

  val reportMap: Seq[ReportCfg] = parseCsv(cfgDir / "ABR-reports.csv", 0)
    .map(r => ReportCfg(
      r.get(0),
      r.get(1),
      r.get(2).trim + nrTailRegexp,
    )).toSeq

  override def transform(n: Node): Seq[Node] = {
    if (n.label != "title") n
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
