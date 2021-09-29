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
package nl.knaw.dans.easy.bag2deposit

import scala.util.Try
import scala.xml.Node

class FoXml {
  // dressed down copy of https://github.com/DANS-KNAW/easy-fedora-to-bag

  private def getStream(streamId: String, rootTag: String, foXml: Node): Try[Node] = Try {
    val node = getStreamRoot(streamId, foXml)
      .filter(hasControlGroup("X"))
      .getOrElse(throw new Exception(s"Stream with ID=$streamId and CONTROL_GROUP=X not found"))

    (node \\ "xmlContent")
      .last
      .descendant
      .filter(_.label == rootTag)
      .last
  }

  private def getStreamRoot(streamId: String, foXml: Node): Option[Node] = {
    (foXml \ "datastream")
      .theSeq
      .filter(n => n \@ "ID" == streamId)
      .lastOption
  }

  def getAmd(foXml: Node): Try[Node] = getStream("AMD", "administrative-md", foXml)

  private def hasControlGroup(controlGroup: String)(streamRoot: Node): Boolean = {
    streamRoot.attribute("CONTROL_GROUP").map(_.text).contains(controlGroup)
  }
}

object FoXml extends FoXml {
}
