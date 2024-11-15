/*
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

object TitlesPerDataset extends App {
  // expects titles extracted from solr in the last column
  // example input line
  // blabla\trabarbera\txxx\, xxx,xxx
  File("data/titles.tsv")
    .lines.toSeq
    .map(_.split("\t"))
    .foreach { a =>
      a.last
        .replaceAll(""""(.*)"""", "$1") // strip quotes
        .split("""\\,""")
        .filter(_.trim.nonEmpty)
        .sortBy(identity)
        .distinct // e.g. easy-dataset:34135, easy-dataset:99840
        .toSeq
        .foreach(t => println(s"${a.init.mkString("\t")}\t $t"))
    }
}
