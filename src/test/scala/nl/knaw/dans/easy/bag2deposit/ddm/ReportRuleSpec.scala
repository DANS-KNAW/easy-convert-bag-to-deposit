package nl.knaw.dans.easy.bag2deposit.ddm

import better.files.File
import nl.knaw.dans.easy.bag2deposit.Fixture.FileSystemSupport
import nl.knaw.dans.easy.bag2deposit.ddm
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReportRuleSpec extends AnyFlatSpec with Matchers with FileSystemSupport {
  private val rule: ReportRewriteRule = ddm.ReportRewriteRule(File("src/main/assembly/dist/cfg"))
  private val uuidToPreferredLabel = rule.reportMap.map(r => r.uuid -> r.label).toMap
  private val identifiers = File("src/test/resources/possibleArchaeologyIdentifiers.txt")
    .lines.filter(_.matches(".*[ (].*"))

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
    transformed
      .map(node => (node \@ "valueURI").replaceAll(".*/", ""))
      .sortBy(identity).distinct
      .map(uuidToPreferredLabel.getOrElse(_, ""))
      .sortBy(identity) shouldBe
      Seq("", "Archol-rapport", "Argo-rapport", "KSP-rapport", "Periplus / Archeomare Rapport", "Rapport")

    transformed.map(node => (node \@ "reportNo")) shouldBe
      List("152", "127", "183", "18382", "A07_A010")
  }
  it should "produce checklists" in {
    val results = identifiers.flatMap(id =>
      rule.transform(<identifier>{ id }</identifier>)
    ).groupBy(_.label)
    (testDir / "identifiers-matched").writeText(
      results("reportNumber").map(_.text).mkString("\n")
    )
    (testDir / "identifiers-missed").writeText(
      results("identifier").map(_.text).mkString("\n")
    )
  }
}
