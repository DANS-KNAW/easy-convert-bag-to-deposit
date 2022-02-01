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

import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.xml.transform.RewriteRule
import scala.xml.{Node, Text}

object UnicodeRewriteRule extends RewriteRule with DebugEnhancedLogging {

  override def transform(node: Node): Seq[Node] = {
    if (!(node.label == "encoded" && node.prefix == "hack")) Seq(node)
    else {
      val bytes = node.nonEmptyChildren.map(n =>
        Integer.parseInt(n.label.substring(1), 16).toByte
      ).toArray
      val str = new String(bytes)
      trace(s"$str ${str.getBytes() sameElements bytes}")
      Seq(Text(str))
    }
  }
}
