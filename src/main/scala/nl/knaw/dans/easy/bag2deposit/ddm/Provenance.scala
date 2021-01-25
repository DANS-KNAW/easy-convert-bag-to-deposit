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

import nl.knaw.dans.easy.bag2deposit.normalized

import scala.xml.Node

object Provenance {
  def apply(generated: Node, modified: Node, version: String): Option[String] = {
    val original = normalized(generated).split("\n")
    val changed = normalized(modified).split("\n")
    val diff1 = original.diff(changed).mkString("\n").trim
    val diff2 = changed.diff(original).mkString("\n").trim
    if (diff1.nonEmpty || diff2.nonEmpty)
      Some(
        s"""===== only in old DDM
           |
           |$diff1
           |
           |===== only in new DDM by $version
           |
           |$diff2
           |""".stripMargin)
    else None
  }
}
