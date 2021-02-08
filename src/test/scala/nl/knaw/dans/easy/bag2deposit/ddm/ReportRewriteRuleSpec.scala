package nl.knaw.dans.easy.bag2deposit.ddm

import better.files.File
import nl.knaw.dans.easy.bag2deposit.Fixture.FileSystemSupport
import nl.knaw.dans.easy.bag2deposit.ddm
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.xml.Node

class ReportRewriteRuleSpec extends AnyFlatSpec with Matchers with FileSystemSupport {
  private val rule: ReportRewriteRule = ddm.ReportRewriteRule(File("src/main/assembly/dist/cfg"))
  private val uuidToPreferredLabel = rule.reportMap.map(r => r.uuid -> r.label).toMap
  private val identifiers = File("src/test/resources/possibleArchaeologyIdentifiers.txt")
    .lines.filter(_.matches(".*[ (].*"))
  private val titlesPerDataset = File("src/test/resources/archeologischeTitels.txt")
    .lines.toSeq
    .map(_.split(",", 2))
    .map { case Array(id, titles) =>
      (
        id,
        titles
          .replaceAll(""""(.*)"""", "$1") // strip quotes
          .replaceAll("""\\,""", "%") // replace real commas
          .split("""(,|\\n *)""")
          .map(_.replaceAll("%", ",")) // restore real commas
          .filter(_.trim.nonEmpty)
          .sortBy(identity)
          .distinct // e.g. easy-dataset:34135, easy-dataset:99840
          .toSeq
      )
    }.toMap
  private lazy val titles = titlesPerDataset.values.flatten.toSeq

  "identifiers" should "return proper numbers" in {
    val transformed = Seq(
      "A07_A010 (Periplus Archeomare rapport)",
      "Archol 145 (rapportnummer)",
      "Archol rapport 152",
      "DHS32 (Archol)",
      "Archol-rapport 127",
      "Rapportnr.: Argo 183",
      "KSP Rapport : 18382",
    ).flatMap(id =>
      rule.transform(<identifier>{ id }</identifier>)
    )
    toPreferredLabel(transformed) shouldBe
      Seq("", "Archol-rapport", "Argo-rapport", "KSP-rapport", "Periplus / Archeomare Rapport", "Rapport")

    transformed.map(node => (node \@ "reportNo")) shouldBe
      List("152", "127", "183", "18382", "A07_A010")
  }

  private def toPreferredLabel(transformed: Seq[Node]) = {
    transformed
      .map(node => (node \@ "valueURI").replaceAll(".*/", ""))
      .sortBy(identity).distinct
      .map(uuidToPreferredLabel.getOrElse(_, ""))
      .sortBy(identity)
  }

  "titles" should "not change" in {
    val input = Seq(
      <title>blabla</title>,
      <title>blablabla : rapport 21</title>,
      <alternative>rabarbera</alternative>,
      <alternative>rabarbera : rapport 21</alternative>,
    )
    input.flatMap(rule.transform) shouldBe input
  }

  it should "return proper numbers" in {
    // difference between titles in the profile/dcmiMedata section are tested with RewriteSpec
    val results = Seq(
      <alternative>rapportnr.: 123</alternative>,
      <title>Rapport 456</title>,
      <alternative>Transect-rapport 2859: Een Archeologisch Bureauonderzoek. Ellecom, glasvezeltracé Eikenstraat, Gemeente Rheden (GD).</alternative>,
    ).flatMap(rule.transform)

    val transformed = results.filter(_.label == "reportNumber")
    transformed
      .map(node => node \@ "reportNo")
      .sortBy(identity) shouldBe
      Seq("123", "2859", "456")
    transformed.map(_.text).sortBy(identity) shouldBe
      Seq("Rapport 456", "Transect-rapport 2859", "rapportnr.")
    results.filter(_.label != "reportNumber").map(_.text) shouldBe
      Seq(
        "rapportnr.: 123", // TODO why treated like <report-nr> : <rest-of-title> ?
        "Transect-rapport 2859: Een Archeologisch Bureauonderzoek. Ellecom, glasvezeltracé Eikenstraat, Gemeente Rheden (GD).",
      )
  }

  it should "produce identifier checklists" in {
    val results = identifiers.flatMap(id =>
      rule.transform(<identifier>{ id }</identifier>)
    ).groupBy(_.label)
    (testDir / "identifiers-matched").writeText(
      results("reportNumber").map(_.text).mkString("\n")
    )
    val missed = results("identifier").groupBy(
      _.text.toLowerCase.matches(".*(notitie|rapport|bericht|publicat).*")
    )
    (testDir / "identifiers-missed-with-keyword").writeText(
      missed(true).map(_.text).mkString("\n")
    )
    (testDir / "identifiers-missed-otherwise").writeText(
      missed(false).map(_.text).mkString("\n")
    )
  }
  it should "produce title checklists" in {
    val results = titles.flatMap(id =>
      rule.transform(<title>{ id }</title>)
    ).groupBy(_.label)
    (testDir / "titles-matched").writeText(
      results("reportNumber").map(_.text).mkString("\n")
    )
    val missed = results("title").groupBy(title => rule.reportMap.exists(cfg =>
      title.text.toLowerCase.matches(".*" + cfg.regexp + ReportRewriteRule.nrRegexp))
    )
    (testDir / "titles-missed-at-end").writeText(
      missed(true).map(_.text).mkString("\n")
    )
    val missedInTheMiddle = missed(true).groupBy(
      _.text.toLowerCase.matches(".*(notitie|rapport|bericht|publicat).*")
    )
    (testDir / "titles-missed-with-keyword").writeText(
      missedInTheMiddle(true).map(_.text).mkString("\n")
    )
    (testDir / "titles-missed-otherwise").writeText(
      missedInTheMiddle(false).map(_.text).mkString("\n")
    )
  }
}
