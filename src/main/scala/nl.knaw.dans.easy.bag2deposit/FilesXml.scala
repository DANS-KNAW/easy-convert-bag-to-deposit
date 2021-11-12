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

import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Elem, NamespaceBinding, Node, NodeSeq, XML}

object FilesXml extends DebugEnhancedLogging {

  val DEFAULT_PREFIX = "dcterms"
  val DEFAULT_URI = "http://purl.org/dc/terms/"

  def apply(filesXml: Elem, destination: String, addedFiles: Seq[String], mimeType: String): Node = {

    val formatTagPrefix = Option(filesXml.scope.getPrefix(DEFAULT_URI)).getOrElse(DEFAULT_PREFIX)
    val binding = NamespaceBinding(formatTagPrefix, DEFAULT_URI, filesXml.scope)
    val format = <xx:format>{ mimeType }</xx:format>
      .copy(prefix = formatTagPrefix)
    val filesXmlWithPossiblyAddedNamespace = Option(filesXml.scope.getURI(formatTagPrefix))
      .map(_ => filesXml)
      .getOrElse(filesXml.copy(scope = binding))
    val newFileElements = addedFiles.map(newFile =>
      <file filepath={s"$destination/$newFile"} >
        { format }
        <accessibleToRights>ANONYMOUS</accessibleToRights>
        <visibleToRights>ANONYMOUS</visibleToRights>
      </file>
    )

    object insertElements extends RewriteRule {
      override def transform(node: Node): Seq[Node] = node match {
        case Elem(boundPrefix, "files", _, boundScope, children@_*) =>
          <files>
            {children}
            {newFileElements}
          </files>.copy(prefix = boundPrefix, scope = boundScope)
        case other => other
      }
    }
    new RuleTransformer(insertElements).transform(filesXmlWithPossiblyAddedNamespace).head
  }
}
