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

  val DCTERMS_PREFIX = "dcterms"
  val DCTERMS_URI = "http://purl.org/dc/terms/"
  val FILES_PREFIX = null
  val FILES_URI = "http://easy.dans.knaw.nl/schemas/bag/metadata/files/"

  def apply(filesXml: Elem, destination: String, addedFiles: Seq[String], mimeType: String): Node = {

    val formatTagPrefix = Option(filesXml.scope.getPrefix(DCTERMS_URI)).getOrElse(DCTERMS_PREFIX)
    val filesBinding = NamespaceBinding(FILES_PREFIX, FILES_URI, filesXml.scope)
    val binding = NamespaceBinding(formatTagPrefix, DCTERMS_URI, filesBinding)
    val format = <xx:format>{ mimeType }</xx:format>
      .copy(prefix = formatTagPrefix)
    val filesXmlWithPossiblyAddedNamespace2 = Option(filesXml.scope.getURI(FILES_PREFIX))
      .map(_ => filesXml)
      .getOrElse(filesXml.copy(scope = filesBinding))
    val filesXmlWithPossiblyAddedNamespace = Option(filesXml.scope.getURI(formatTagPrefix))
      .map(_ => filesXmlWithPossiblyAddedNamespace2)
      .getOrElse(filesXmlWithPossiblyAddedNamespace2.copy(scope = binding))
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
