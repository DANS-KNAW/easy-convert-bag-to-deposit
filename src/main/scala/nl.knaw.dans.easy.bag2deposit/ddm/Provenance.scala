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

import scala.xml.Utility.trim
import scala.xml._

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
  def compareAMD(oldXml: Node, newXml: Node): Option[Elem] = {
    compareNodeSeq(amdGrandChildren(trim(oldXml)), amdGrandChildren(trim(newXml)), oldXml.scope, "http://easy.dans.knaw.nl/easy/dataset-administrative-metadata/")
  }

  def compareDDM(oldXml: Node, newXml: Node): Option[Elem] = {
    compareNodeSeq(ddmGrandChildren(trim(oldXml)), ddmGrandChildren(trim(newXml)), oldXml.scope, "http://easy.dans.knaw.nl/schemas/md/ddm/")
  }

  private def amdGrandChildren(xml: Node) = {
    // we don't want all children of stateChangeDates when one of them changed
    // but we do want the siblings of stateChangeDates when changed
    xml.nonEmptyChildren.theSeq.filter(_.label != "stateChangeDates") ++
    (xml \ "stateChangeDates").flatMap(_.nonEmptyChildren).theSeq
  }

  private def ddmGrandChildren(xml: Node) = {
    // we don't want all grandchildren when one of them changed
    (xml \ "profile").flatMap(_.nonEmptyChildren).theSeq ++
      (xml \ "dcmiMetadata").flatMap(_.nonEmptyChildren).theSeq
  }

  private def compareNodeSeq(oldNodes: Seq[Node], newNodes: Seq[Node], scope: NamespaceBinding, scheme: String) = {
    val onlyInOld = oldNodes.diff(newNodes).filter(_.nonEmpty)
    val onlyInNew = newNodes.diff(oldNodes).filter(_.nonEmpty)
    if (onlyInOld.isEmpty && onlyInNew.isEmpty) None
    else {
      val old = if (onlyInOld.isEmpty) Text("")
                else PCData(onlyInOld.mkString("\n"))
      Some(
        <prov:file scheme={ scheme }>
        <prov:old>{ old }</prov:old>
        <prov:new>{ onlyInNew }</prov:new>
      </prov:file>.copy(scope = scope) // TODO in case of ddm perhaps also dc[t[erms]] and dcx-gml?
      )
    }
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
