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
import nl.knaw.dans.easy.bag2deposit.parseCsv

import scala.xml.Node
import scala.xml.transform.RewriteRule

case class LanguageRewriteRule(cfgFile: File) extends RewriteRule {

  private val languageMap = parseCsv(cfgFile, 0).toSeq
    .map(r => r.get(0) -> r.get(1).split(",").toSeq)
    .toMap

  override def transform(node: Node): Seq[Node] = {
    if (node.label != "language" || isProperLang(node))
      node
    else node +: languageMap.getOrElse(node.text, Seq.empty)
      .map(lang =>
        <ddm:language encodingScheme='ISO639-2' code={ lang }>{ node.text }</ddm:language>
      )
  }

  private def isProperLang(node: Node) = {
    node.attributes.prefixedKey == "xsi:type" && node.attributes.mkString("") == "dcterms:ISO639-2"
  }
}
