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
    ".*archis ?2?[^a-z](vondst?|aan)?melding.*" -> "VONDSTMELDING",
    ".*archis ?2?[^a-z]waarneming.*" -> "WAARNEMING",
    ".*archis ?2?[^a-z]onderzoek.*" -> "ONDERZOEK",
    ".*onderzoek[a-z]*[^a-z]archis ?2?.*" -> "ONDERZOEK",
    ".*archis ?2?[^a-z]monument.*" -> "MONUMENT",
    ".*archis ?2?[^a-z]zaak[^a-z]identificatie.*" -> "ZAAK-IDENTIFICATIE",
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

    def splitTypedNrs = {
      node.text.split("[,;] *").map(nr =>
        node.asInstanceOf[Elem].copy(child = new Text(nr)
        ))
    }

    def isTypedArchis = {
      node.attributes.toString.contains("id-type:ARCHIS-")
    }

    def isPlainArchis = {
      node.text.toLowerCase.matches(".*[(].*archis.*")
    }

    def splitPlainNrs = {
      val Array(nrs, trailer) = node.text.split(" *[(]", 2)
      val strings = nrs.split("[,;] *")
      regexpMap
        .find { case (regexp, _) =>
          trailer.toLowerCase().matches(regexp)
        }
        .map { case (_, value) => typedIds(strings, typeAttr(value)) }
        .getOrElse(plainIds(strings, trailer))
    }

    if (node.label != "identifier") node
    else if (isTypedArchis) splitTypedNrs
         else if (isPlainArchis) splitPlainNrs
              else node
  }
}

