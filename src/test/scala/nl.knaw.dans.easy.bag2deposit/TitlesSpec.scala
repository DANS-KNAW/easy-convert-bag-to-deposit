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
import nl.knaw.dans.easy.bag2deposit.Fixture.FileSystemSupport
import nl.knaw.dans.easy.bag2deposit.ddm.ReportRewriteRule
import org.scalatest.flatspec.AnyFlatSpec

class TitlesSpec extends AnyFlatSpec with FileSystemSupport {
  private val rule: ReportRewriteRule = ddm.ReportRewriteRule(File("src/main/assembly/dist/cfg"))
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

  it should "show number of matches per RCE" ignore {
    rule.reportMap.foreach { m =>

      val n = titlesPerDataset
        .mapValues(_.filter(_.toLowerCase.matches(m.regexp)))
        .count(_._2.nonEmpty)
      println(s"$n : \t${ m.label }")
    }
  }
  it should "show matches and missed" in {
    (testDir / "matches-per-rce.txt").write(rule.reportMap.map { m =>
      titlesPerDataset
        .mapValues(_.filter(_.toLowerCase.matches(m.regexp)))
        .filter(_._2.nonEmpty)
        .map { case (id, t) => t.map(t => s"\n\t$id\t--- $t").mkString("\n") }
        .toSeq.mkString(s"${ m.label }", "", "\n")
    }.mkString(""))

    (testDir / "missed-at-end-of-title.txt").write(rule.reportMap.filterNot(_.label == "Rapport").map { m =>
      titlesPerDataset
        .mapValues(_.filter(_.toLowerCase.matches(".+" + m.regexp)))
        .filter(_._2.nonEmpty)
        .map { case (id, t) => t.map(t => s"\n\t$id\t--- $t").mkString("\n") }
        .toSeq.mkString(s"${ m.label }", "", "\n")
    }.mkString(""))

    val keyword = "(notitie|rapport|bericht|publicat)"
    println(rule.reportMap.map(_.label)
      .filterNot(_.toLowerCase.matches(s".*$keyword.*"))
      .zipWithIndex.mkString("without keyword:\n\t", "\n\t", "")
    )
    (testDir / "missed.txt").write(titlesPerDataset.map { case (id, t) =>
      t.filterNot(title => rule.reportMap.exists(m => title.toLowerCase.matches(".*" + m.regexp)))
        .filter(_.toLowerCase.matches(s".*$keyword[^0-9]*${ rule.nrRegexp }(:.*)?"))
        .map(title => s"\n$id\t$title")
        .mkString("")
    }.mkString(""))

    (testDir / "briefrapport.txt").write(
      (testDir / "missed.txt")
        .lines
        .filter(_.toLowerCase.matches(s".*brief[^a-z]*rapport${rule.nrRegexp}.*"))
        .mkString("\n")
    )
  }
}
