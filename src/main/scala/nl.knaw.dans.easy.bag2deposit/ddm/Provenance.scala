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

import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.joda.time.DateTime.now
import org.joda.time.format.DateTimeFormat

import scala.xml.{ Elem, Node }

class Provenance(app: String, version: String) extends DebugEnhancedLogging{
  private val dateFormat = now().toString(DateTimeFormat.forPattern("yyyy-MM-dd"))

  def xml(changes: Map[String, Seq[Node]]): Option[Elem] = {
    trace(this.getClass)
    val filtered = changes.filter(_._2.nonEmpty)
    if (filtered.isEmpty) None
    else Some(
      <prov:provenance xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
        xmlns:prov="http://easy.dans.knaw.nl/schemas/bag/metadata/prov/"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xmlns:dc="http://purl.org/dc/elements/1.1/"
        xmlns:dct="http://purl.org/dc/terms/"
        xsi:schemaLocation="
        http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd
        http://www.loc.gov/mods/v3 http://www.loc.gov/standards/mods/v3/mods-3-7.xsd
        http://easy.dans.knaw.nl/schemas/bag/metadata/prov/ https://easy.dans.knaw.nl/schemas/bag/metadata/prov/provenance.xsd
        ">
        <prov:migration app={ app } version={ version } date={ now().toString(dateFormat) }>
          { filtered.map { case (scheme, diff) =>
          <prov:file scheme={ scheme }>{ diff }</prov:file>
        }}
        </prov:migration>
      </prov:provenance>
    )
  }
}
object Provenance {
  def compare(oldXml: Node, newXml: Node): Seq[Node] = {

    // children of both profile and dcmiMetadata
    val oldNodes = oldXml.flatMap(_.nonEmptyChildren).flatMap(_.nonEmptyChildren)
    val newNodes = newXml.flatMap(_.nonEmptyChildren).flatMap(_.nonEmptyChildren)
    val onlyInOld = oldNodes.diff(newNodes)
    val onlyInNew = newNodes.diff(oldNodes)

    if (onlyInOld.isEmpty && onlyInNew.isEmpty) Seq.empty
    else <prov:old>{ onlyInOld }</prov:old>
         <prov:new>{ onlyInNew }</prov:new>
  }
}
