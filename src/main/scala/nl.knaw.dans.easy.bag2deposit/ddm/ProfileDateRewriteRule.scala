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

object ProfileDateRewriteRule extends RewriteRule {
  val DDM_NAMESPACE = "http://easy.dans.knaw.nl/schemas/md/ddm/"

  override def transform(node: Node): Seq[Node] = node match {
    case e: Elem if (e.label == "created" || e.label == "available") && e.namespace == DDM_NAMESPACE =>
      if (e.text.trim.length == 7)
        e.copy(child = Text(e.text.trim + "-01"))
      else if (e.text.trim.length == 4)
             e.copy(child = Text(e.text.trim + "-01-01"))
           else e
    case other => other
  }
}
