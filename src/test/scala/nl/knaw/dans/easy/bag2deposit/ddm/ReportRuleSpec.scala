package nl.knaw.dans.easy.bag2deposit.ddm

import better.files.File
import nl.knaw.dans.easy.bag2deposit.ddm
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReportRuleSpec extends AnyFlatSpec with Matchers {
  private val rule: ReportRewriteRule = ddm.ReportRewriteRule(File("src/main/assembly/dist/cfg"))
  private val uuidToPreferredLabel = rule.reportMap.map(r => r.uuid -> r.label).toMap
  private val identifiers = File("src/test/resources/possibleArchaeologyIdentifiers.txt")
    .lines // reducing 48713 to 20448 TODO what about "rapportcode" and ISBDs?
    .filterNot(_.toLowerCase.matches(".*(isbn|project|vindplaats|code).*"))
    .filter(_.toLowerCase.matches("([a-z]+ .*|.* [a-z]+|.*[( ][a-z]+[ )].*)")) // a word at the start/end in the middle
  // File("target/tmp.txt").writeText(identifiers.mkString("\n"))

  "identifiers" should "return proper numbers" in {
    val transformed = Seq(
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
      .map(uuidToPreferredLabel.getOrElse(_, "")) shouldBe
      Seq("", "KSP-rapport", "Argo-rapport", "Archol-rapport")

    transformed.map(node => (node \@ "reportNo")) shouldBe
      List("152", "127", "183", "18382")
  }
}
