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
  private val cfg = rule.reportMap
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

  // a special single one easy-dataset:99840 "Arcadis Archeologische Rapporten [2017 - 116]"
  private val nrRegExp = "\\W+[-_/a-z0-9]+[.]? *$"

  it should "show number of matches per RCE" ignore {
    cfg.foreach { m =>

      val n = titlesPerDataset
        .mapValues(_.filter(_.toLowerCase.matches(m.regexp)))
        .count(_._2.nonEmpty)
      println(s"$n : \t${m.label}")
    }
  }
  it should "show matches and missed" in {
    (testDir / "matches-per-rce.txt").write(cfg.map { m =>
      titlesPerDataset
        .mapValues(_.filter(_.toLowerCase.matches(m.regexp)))
        .filter(_._2.nonEmpty)
        .map { case (id, t) => t.map(t => s"\n\t$id\t--- $t").mkString("\n") }
        .toSeq.mkString(s"${ m.label }", "", "\n")
    }.mkString(""))

    val keyword = "(notitie|rapport|bericht|publicat)"
    println(cfg.map(_.label)
      .filterNot(_.toLowerCase.matches(s".*$keyword.*"))
      .zipWithIndex.mkString("without keyword:\n\t","\n\t","")
    )
    (testDir / "missed.txt").write(titlesPerDataset.map { case (id, t) =>
      t.filterNot(title => cfg.exists(m => title.toLowerCase.matches(m.regexp)))
        .filter(_.toLowerCase.matches(s".*$keyword[^0-9]*${ rule.nrRegExp }(:.*)?"))
        .map(title => s"\n$id\t$title")
        .mkString("")
    }.mkString(""))
  }
  it should "show general"  ignore {
    // easy-dataset:103605,"...Zuidnederlandse Archeologische Rapporten 62..." heeft al een identifier: "ZAR 62"

    val mnemonics = "adc|baac|gra|mug|vestigia|zan|zar"
    val prefix = s"(ar(ch|g)a?eo[a-z]*|$mnemonics)"
    val suffix = "([a-z]*rapport|[a-z]*notitie|bericht)[a-z]*"
    val rapporten = titlesPerDataset.values.toSeq
      .flatMap(title => title
        .filter(_.matches(s".*$nrRegExp"))
        .map(_.toLowerCase.replaceAll(nrRegExp, "").replace(" [2017 - 116]", ""))
        .filter(_.matches(s"(.*$prefix[ -]$suffix.*|$mnemonics)"))
      ).sortBy(identity)
      .distinct

    println(rapporten.mkString(s"${ rapporten.size }\n", "\n", ""))
  }
  it should "convert" ignore {
    // easy-dataset:41345,"Plangebied -¦t Hart
    // easy-dataset:19336,"Bureauonderzoek en inventariserend veldonderzoek Pingo-ruíne
    parseCsv(File("src/main/assembly/dist/cfg/ABR-reports.csv"), nrOfHeaderLines = 0)
      .foreach { r =>
        val regexp = r.get(1).toLowerCase
          // [^ -~\n‘’“”±áàäâçčćñčé̈èeêëïíóôöüûuúšžĐ¿»«Ø–‐¦¬\t„‟‘’“”´″·…] and 25 other odd characters
          .replaceAll("""[ "/(),-]+""", "[^a-záàäâçčćèeêëïíóôöüûuú0-9]+")
          .replaceAll("archeo[a-z]*", "ar(ch|g)a?eo[a-z]*")
          .replaceAll("rapport[a-z]*", "rapport[a-z]*")
          .replaceAll("notitie[a-z]*", "notitie[a-z]*")
          .replaceAll("publicaties?", "publicaties?")
        println(s"""${ r.get(0) },"${ r.get(1) }","$regexp"""")
      }
  }
}
