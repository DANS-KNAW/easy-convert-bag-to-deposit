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
package nl.knaw.dans.easy.bag2deposit.Fixture

import scala.xml.{ Node, PrettyPrinter, Utility }

trait XmlSupport {
  private val nameSpaceRegExp = """ xmlns:[a-z-]+="[^"]*"""" // these attributes have a variable order
  val printer = new PrettyPrinter(160, 2) // Utility.serialize would preserve white space, now tests are better readable

  def normalized(elem: Node): String = printer
    .format(Utility.trim(elem)) // this trim normalizes <a/> and <a></a>
    .replaceAll(nameSpaceRegExp, "") // the random order would cause differences in actual and expected
    .replaceAll(" +\n?", " ")
    .replaceAll("\n +<", "\n<")
    .trim
}
