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
import scala.xml.{ Elem, Node, Text }

object SplitNrRewriteRule extends RewriteRule {

  override def transform(node: Node): Seq[Node] = {
    node match {
      case Elem(_, "identifier", _, _, Text(value)) if node.attributes.toString.contains("id-type:ARCHIS-") =>
        value.split("[,;] *").map(nr =>
          node.asInstanceOf[Elem].copy(child = new Text(nr)
        ))
      case Elem(_, "identifier", _, _, Text(value)) if value.toLowerCase.matches(".*;.*[(]archis.*") =>
        val Array(nrs, trailer) = value.split(" *[(]", 2)
        nrs.split("[,;] *").map(nr =>
          node.asInstanceOf[Elem].copy(child = new Text(s"$nr ($trailer"))
        )
      case _ => node
    }
  }
}

