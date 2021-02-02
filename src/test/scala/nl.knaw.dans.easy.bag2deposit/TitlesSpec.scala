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
import nl.knaw.dans.easy.bag2deposit.ddm.ReportRewriteRule.nrRegexp
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
  private lazy val titles = titlesPerDataset.values.flatten.toSeq

  it should "show number of matches per RCE" ignore {
    rule.reportMap.foreach { m =>

      val n = titlesPerDataset
        .mapValues(_.filter(_.toLowerCase.matches(m.regexp)))
        .count(_._2.nonEmpty)
      (testDir / "number-of-matches-per-rce.txt").write(s"$n : \t${ m.label }")
    }
  }

  it should "show matches and missed report numbers" in {
    // slow because of various cross references of 135 regular expression
    // against 101322 distinct titles of 48675 archaeological datasets
    // TODO manually analyse generated files to analyse effectiveness and correctness of regexps in ABR-report.csv
    val titleToReport = titles.map { title =>
      title -> rule.reportMap.filter(m => title.trim.toLowerCase.matches(m.regexp)).map(_.label)
    }.toMap

    val found = titleToReport
      .filter(_._2.nonEmpty)
      .groupBy(_._2)

    (testDir / "titles-per-report.txt").write(
      found.map { case (report, titleToReport) =>
        titleToReport.keys.toSeq.sortBy(_.toLowerCase).distinct
          .mkString(s"${ report.mkString("\t") }\n\t", "\n\t", "")
      }.mkString("\n")
    )

    val regexpToLabel = rule.reportMap.filterNot(_.label == "Rapport")
      .map(m => ".+" + m.regexp -> m.label)
      .toMap

    val missedTitles = titleToReport
      .filter(_._2.isEmpty)
      .keys.toSeq

    val missedLabelToTitles = missedTitles
      .groupBy(title =>
        regexpToLabel
          .find(rToL => title.toLowerCase.matches(rToL._1))
          .map(_._2)
      )

    (testDir / "missed-at-end-of-title.txt").write(
      missedLabelToTitles
        .filter(_._1.isDefined)
        .map { case (Some(label), titles) =>
          titles.mkString(s"$label\n\t", "\n\t", "")
        }.mkString("\n")
    )

    val otherMissed = missedLabelToTitles
      .filter(_._1.isEmpty)
      .values.toList.flatten
      .groupBy(_.toLowerCase.matches(".*(notitie|rapport|bericht|publicat).*"))

    (testDir / "without-missed-keyword.txt").write(
      otherMissed(false).mkString("\n")
    )
    (testDir / "missed.txt").write(
      otherMissed(true).mkString("\n")
    )

    (testDir / "briefrapport.txt").write(
      (testDir / "missed.txt")
        .lines
        .filter(_.toLowerCase.matches(s".*brief[^a-z]*rapport${ nrRegexp }.*"))
        .mkString("\n")
    )
  }
}
