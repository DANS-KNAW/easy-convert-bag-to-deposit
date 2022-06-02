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
import org.joda.time.DateTime.now
import org.joda.time.format.DateTimeFormat

import scala.xml.{ Elem, Node, PCData, Utility }

case class Provenance(app: String, version: String, schemaRoot: String = "https://easy.dans.knaw.nl/schemas") extends DebugEnhancedLogging {
  private val schemaLocation = s"http://easy.dans.knaw.nl/schemas/bag/metadata/prov/ $schemaRoot/bag/metadata/prov/provenance.xsd"
  private val dateFormat = now().toString(DateTimeFormat.forPattern("yyyy-MM-dd"))

  def collectChangesInXmls(maybeChanges: Seq[Option[Elem]]): Elem = {
    trace(this.getClass)
    <prov:provenance xmlns:prov="http://easy.dans.knaw.nl/schemas/bag/metadata/prov/"
                     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                     xsi:schemaLocation={  schemaLocation }>
        <prov:migration app={ app } version={ version } date={ now().toString(dateFormat) }>
        { maybeChanges.filter(_.nonEmpty).flatMap(_.toSeq) }
        </prov:migration>
    </prov:provenance>
  }
}
object Provenance extends DebugEnhancedLogging {
  /**
   * Creates the content for a <prov:migration> by comparing the direct child elements of each XML.
   * @param oldXml the original instance
   * @param newXml the modified instance
   * @return and empty list if both versions have the same children
   *         when large/complex elements (like for example authors or polygons) have minor changes
   *         both versions of the complete element is returned
   */
  def compare(oldXml: Node, newXml: Node, scheme: String): Option[Elem] = {
    // TODO poor mans solution to call with ddm/dcmiMetadata respective root of amd
    val oldRootNodes = Utility.trim(oldXml).flatMap(_.nonEmptyChildren)
    val newRootNodes = Utility.trim(newXml).flatMap(_.nonEmptyChildren)
    val oldStateNodes = oldRootNodes.find(_.label == "stateChangeDates").toSeq.flatten.flatMap(_.nonEmptyChildren)
    val newStateNodes = newRootNodes.find(_.label == "stateChangeDates").toSeq.flatten.flatMap(_.nonEmptyChildren)
    val newSimpleNodes = newRootNodes.filterNot(_.label == "stateChangeDates")
    val oldSimpleNodes = oldRootNodes.filterNot(_.label == "stateChangeDates")
    val newNodes = newSimpleNodes.theSeq ++ newStateNodes.theSeq
    val oldNodes = oldSimpleNodes.theSeq ++ oldStateNodes.theSeq
    val onlyInOld = oldNodes.diff(newNodes).filter(_.nonEmpty)
    val onlyInNew = newNodes.diff(oldNodes).filter(_.nonEmpty)

    if (onlyInOld.isEmpty && onlyInNew.isEmpty) None
    else Some(
      <prov:file scheme={ scheme }>
        <prov:old>{ onlyInOld }</prov:old>
        <prov:new>{ onlyInNew }</prov:new>
      </prov:file>.copy(scope = oldXml.scope) // TODO in case of ddm perhaps also dc[t[erms]] and dcx-gml?
    )
  }

  def fixedDdmEncoding(oldEncoding: Seq[String], newEncoding: Seq[String]): Option[Elem] = {
    if (oldEncoding.isEmpty) None
    else Some(
      <prov:file filename="dataset.xml">
        <prov:old>
          <prov:encoding>{PCData(oldEncoding.zipWithIndex.map {case (s,i) => s"$i:$s" }.mkString(" "))}</prov:encoding>
        </prov:old>
        <prov:new>
          <prov:encoding>{newEncoding.zipWithIndex.map {case (s,i) => s"$i:$s" }.mkString(" ")}</prov:encoding>
        </prov:new>
      </prov:file>
    )
  }
}
