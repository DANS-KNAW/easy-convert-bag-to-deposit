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

import scala.xml.transform.RewriteRule
import scala.xml.{ Elem, Node, PrefixedAttribute, Text }

object SplitNrRewriteRule extends RewriteRule {

  val regexpMap = Seq(
    ".*archis[^a-z]vondstmelding.*" -> "VONDSTMELDING",
    ".*archis[^a-z]waarneming.*" -> "WAARNEMING",
    ".*archis[^a-z]monument.*" -> "ONDERZOEK",
    ".*archis[^a-z]onderzoek.*" -> "VONDSTMELDING",
    ".*archis[^a-z]onderzoek.*" -> "ZAAK-IDENTIFICATIE",
  )

  override def transform(node: Node): Seq[Node] = {

    def typeAttr(value: String) = {
      new PrefixedAttribute("xsi", "type", s"id-type:ARCHIS-$value", node.attributes)
    }

    def typedIds(strings: Array[String], attr: PrefixedAttribute): Seq[Elem] = {
      strings.map(nr =>
        node.asInstanceOf[Elem].copy(child = new Text(nr), attributes = attr)
      )
    }

    def plainIds(strings: Array[String], trailer: String): Seq[Elem] = {
      strings.map(nr =>
        node.asInstanceOf[Elem].copy(child = new Text(s"$nr ($trailer"))
      )
    }

    node match {
      case Elem(_, "identifier", _, _, Text(value)) if isTypedArchis(node) =>
        value.split("[,;] *").map(nr =>
          node.asInstanceOf[Elem].copy(child = new Text(nr)
          ))
      case Elem(_, "identifier", _, _, Text(value)) if isPlainArchis(value) =>
        val Array(nrs, trailer) = value.split(" *[(]", 2)
        val strings = nrs.split("[,;] *")
        regexpMap
          .find { case (regexp, _) =>
            trailer.toLowerCase().matches(regexp)
          }
          .map { case (_, value) => typedIds(strings, typeAttr(value)) }
          .getOrElse(plainIds(strings, trailer))
      case _ => node
    }
  }

  private def isTypedArchis(node: Node) = {
    node.attributes.toString.contains("id-type:ARCHIS-")
  }

  private def isPlainArchis(value: String) = {
    value.toLowerCase.matches(".*;.*[(]archis.*")
  }
}

