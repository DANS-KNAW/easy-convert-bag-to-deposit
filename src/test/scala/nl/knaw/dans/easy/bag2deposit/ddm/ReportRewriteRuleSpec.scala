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
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReportRewriteRuleSpec extends AnyFlatSpec with Matchers with FileSystemSupport {
  private val rule: ReportRewriteRule = ddm.ReportRewriteRule(File("src/main/assembly/dist/cfg"))

  "transform <identifier>" should "convert" in {
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
}
