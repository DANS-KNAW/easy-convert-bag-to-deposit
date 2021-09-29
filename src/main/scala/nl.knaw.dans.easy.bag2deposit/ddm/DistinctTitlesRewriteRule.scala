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

import nl.knaw.dans.easy.bag2deposit.ddm.DistinctTitlesRewriteRule.distinctTitles

import scala.xml.{ Elem, Node }
import scala.xml.transform.RewriteRule

case class DistinctTitlesRewriteRule(profileTitle: String) extends RewriteRule {
  override def transform(node: Node): Seq[Node] = {
    if (node.label != "dcmiMetadata") node
    else node.asInstanceOf[Elem]
      .copy(child = distinctTitles(profileTitle, node.nonEmptyChildren))
  }
}
object DistinctTitlesRewriteRule {
  def distinctTitles(profileTitle: String, dcmiChildren: Seq[Node]): Seq[Node] = {
    val (titles, others) = dcmiChildren.partition(node => Seq("title","alternative").contains(node.label))
    val distinctTitles = titles.sortBy(_.text).distinct
    val titleValues = distinctTitles.map(_.text) :+ profileTitle
    distinctTitles
      .filterNot { node =>
        val text = node.text
        titleValues.filterNot(_ == text).exists(_.contains(text))
      } ++ others
  }
}

