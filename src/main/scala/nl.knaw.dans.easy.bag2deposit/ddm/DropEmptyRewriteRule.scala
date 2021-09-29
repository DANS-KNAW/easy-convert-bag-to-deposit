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

import scala.xml.transform.RewriteRule
import scala.xml.{ Elem, Node, Text }

object DropEmptyRewriteRule extends RewriteRule {
  private val labels = Seq(
    "linkedRelation",
    "conformsTo",
    "hasFormat",
    "hasPart",
    "hasVersion",
    "isFormatOf",
    "isPartOf",
    "isReferencedBy",
    "isReplacedBy",
    "isRequiredBy",
    "isVersionOf",
    "references",
    "replaces",
    "requires",
  )

  override def transform(node: Node): Seq[Node] = {
    if (node.text.trim.nonEmpty) node
    else if (!labels.contains(node.label)) node
         else {
           val href = node.attribute("href").toSeq.flatten.text
           if (href.isEmpty) Seq.empty
           else node.asInstanceOf[Elem].copy(child = new Text(href))
         }
  }
}

