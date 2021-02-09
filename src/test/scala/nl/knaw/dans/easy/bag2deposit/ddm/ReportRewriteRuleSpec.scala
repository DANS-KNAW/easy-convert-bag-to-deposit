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
import nl.knaw.dans.easy.bag2deposit.Fixture.FileSystemSupport
import nl.knaw.dans.easy.bag2deposit.ddm
import nl.knaw.dans.easy.bag2deposit.ddm.ReportRewriteRule.nrRegexp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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

  "transform <identifier>" should "convert haags" in {
    val input = Seq(
      "1503 (Haagse Archeologische Rapportage)",
      "A07_A010 (Periplus Archeomare rapport)",
      "Archol-rapport 127",
      "Archol rapport 152",
      "KSP Rapport : 18382",
    ).map(s => <identifier>{ s }</identifier>)

    input.flatMap(rule.transform)
      .map(_ \@ "reportNo") shouldBe
      Seq("1503", "A07_A010", "127", "152", "18382")
  }
  it should "not convert" in {
    val input = Seq(
      "Archol 145 (rapportnummer)",
      "DHS32 (Archol)",
      "Rapportnr.: Argo 183",
      // "rapportnr.:" only 7 times in identifiers, one time in titles
    ).map(s => <identifier>{s}</identifier>)

    input.flatMap(rule.transform) shouldBe input
  }

  "transform <title>" should "not change" in {
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
      <alternative>rapportnr. 123</alternative>,
      <title>Rapport 456</title>,
      <alternative>Transect-rapport 2859: Een Archeologisch Bureauonderzoek. Ellecom, glasvezeltracé Eikenstraat, Gemeente Rheden (GD).</alternative>,
    ).flatMap(rule.transform)

    val transformed = results.filter(_.label == "reportNumber")
    transformed
      .map(node => node \@ "reportNo")
      .sortBy(identity) shouldBe
      Seq("123", "2859", "456")
    transformed.map(_.text).sortBy(identity) shouldBe
      Seq(
        "Rapport 456",
        "Transect-rapport 2859",
        "rapportnr. 123"
      )
    results.filter(_.label != "reportNumber").map(_.text) shouldBe
      Seq(
        "Transect-rapport 2859: Een Archeologisch Bureauonderzoek. Ellecom, glasvezeltracé Eikenstraat, Gemeente Rheden (GD).",
      )
  }

  "transform" should "produce identifier checklists" in {
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
    // this test takes a minute
    // keep it last to interrupt interactive test cycles
    // while executing all other tests
    val results = titles.flatMap(id =>
      rule.transform(<title>{ id }</title>)
    ).groupBy(_.label)
    (testDir / "titles-matched").writeText(
      results("reportNumber").map(_.text).mkString("\n")
    )
    val missed = results("title").groupBy(title => rule.reportMap.exists(cfg =>
      title.text.toLowerCase.matches(s".*${ cfg.regexp } +$nrRegexp"))
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
