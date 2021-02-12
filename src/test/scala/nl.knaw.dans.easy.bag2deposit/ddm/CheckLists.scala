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
import nl.knaw.dans.easy.bag2deposit.ddm
import nl.knaw.dans.easy.bag2deposit.ddm.ReportRewriteRule.nrRegexp

object CheckLists extends App {
  private val rule: ReportRewriteRule = ddm.ReportRewriteRule(File("src/main/assembly/dist/cfg"))
  private val testDir = File("target/checklists")

  def identifiersLists = {
    val identifiers = File("src/test/resources/possibleArchaeologyIdentifiers.txt")
      .lines.filter(_.matches(".*[ (].*"))
    val (matched, missed) = identifiers
      .flatMap(id => rule.transform(<identifier>{ id }</identifier>))
      .partition(_.label == "reportNumber")
    val (withKeyword, noKeyword) = missed
      .partition(
        _.text.toLowerCase.matches(".*(notitie|rapport|bericht|publicat|synthegra|grontmij).*")
      )

    (testDir / "identifiers-matched").writeText(
      matched.map(_.text).mkString("\n")
    )
    (testDir / "identifiers-missed-with-keyword").writeText(
      withKeyword.map(_.text).mkString("\n")
    )
    (testDir / "identifiers-missed-otherwise").writeText(
      noKeyword.map(_.text).mkString("\n")
    )
  }

  def titlesLists = {
    val titlesPerDataset = File("src/test/resources/archeologischeTitels.txt")
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
    lazy val titles = titlesPerDataset.values.flatten.toSeq
    val results = titles.flatMap(id =>
      // a title with more than a report may follow the new element
      rule.transform(<title>{ id }</title>).head
    ).groupBy(_.label)
    (testDir / "titles-matched").writeText(
      results("reportNumber").map(_.text).mkString("\n")
    )
    val missed = results("title").groupBy(title => rule.reportMap.exists(cfg =>
      title.text.toLowerCase.matches(s".*${ cfg.regexp }[^a-z]+ $nrRegexp"))
    )
    (testDir / "titles-missed-at-end").writeText(
      missed(true).map(_.text).mkString("\n")
    )
    val missedInTheMiddle = missed(false).groupBy(
      _.text.toLowerCase.matches(".*(notitie|rapport|bericht|publicat).*")
    )
    (testDir / "titles-missed-with-keyword").writeText(
      missedInTheMiddle(true).map(_.text).mkString("\n")
    )
    (testDir / "titles-missed-otherwise").writeText(
      missedInTheMiddle(false).map(_.text).mkString("\n")
    )
  }

  if (testDir.exists) testDir.delete()
  testDir.createDirectories()
  identifiersLists
  titlesLists
}
