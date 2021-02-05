package nl.knaw.dans.easy.bag2deposit.ddm

import better.files.File
import nl.knaw.dans.easy.bag2deposit.ddm
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReportRuleSpec extends AnyFlatSpec with Matchers {
  private val rule: ReportRewriteRule = ddm.ReportRewriteRule(File("src/main/assembly/dist/cfg"))
  private val identifiers = File("src/test/resources/possibleArchaeologyIdentifiers.txt").lines
  private val uuidToPreferredLabel = rule.reportMap.map(r => r.uuid -> r.label).toMap
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
      Seq("", "Archol-rapport") // TODO "Argo-rapport", "KSP-rapport"

    transformed.map(node => (node \@ "reportNo")) shouldBe
      List("152", "127")
  }
}
