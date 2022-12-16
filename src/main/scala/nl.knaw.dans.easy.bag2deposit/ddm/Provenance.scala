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

import nl.knaw.dans.easy.bag2deposit.ddm.Provenance.{ createSchemaLocation, hasDdmV2 }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.joda.time.DateTime.now
import org.joda.time.format.DateTimeFormat

import scala.xml.Utility.trim
import scala.xml._

case class Provenance(app: String, version: String) extends DebugEnhancedLogging {
  private val dateFormat = now().toString(DateTimeFormat.forPattern("yyyy-MM-dd"))

  def collectChangesInXmls(maybeChanges: Seq[Option[Elem]]): Elem = {
    trace(this.getClass)
    val changes = maybeChanges.filter(_.nonEmpty).flatMap(_.toSeq)
    val schemaLocation = createSchemaLocation(hasDdmV2(changes))
    <prov:provenance xsi:schemaLocation={ schemaLocation }
                     xmlns:prov={ schemaLocation.split(" ").head }
                     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    >
        <prov:migration app={ app } version={ version } date={ now().toString(dateFormat) }>
        { changes }
        </prov:migration>
    </prov:provenance>
  }
}
object Provenance extends DebugEnhancedLogging {

  private def hasDdmV2(changes: Seq[Elem]) = {
    val fileSchemes = changes.flatMap(n => (n \\ "file").flatMap(_.attribute("scheme"))).flatten
    fileSchemes.exists(_.text.contains("ddm-v2"))
  }

  private def createSchemaLocation(hasV2: Boolean) = {

    val path = "http://schemas.dans.knaw.nl/bag/metadata"
    if (hasV2)
      s"$path/prov-v2/ $path/prov/v2/provenance.xsd"
    else s"http://easy.dans.knaw.nl/schemas/md/ddm/ $path/prov/v1/provenance.xsd"
  }

  /**
   * Creates the content for a <prov:migration> by comparing the direct child elements of each XML.
   * @param oldXml the original instance
   * @param newXml the modified instance
   * @return and empty list if both versions have the same children
   *         when large/complex elements (like for example authors or polygons) have minor changes
   *         both versions of the complete element is returned
   */
  def compareAMD(oldXml: Node, newXml: Node): Option[Elem] = {
    compareNodeSeq(amdGrandChildren(trim(oldXml)), amdGrandChildren(trim(newXml)), newXml.scope, newXml.scope.getURI("amd"))
  }

  def compareDDM(oldXml: Node, newXml: Node): Option[Elem] = {
    logger.debug(s"OLD SCOPE: ${oldXml.scope.getURI("ddm")} -- ${oldXml.scope}")
    logger.debug(s"NEW SCOPE: ${newXml.scope.getURI("ddm")} -- ${newXml.scope}")
    compareNodeSeq(ddmGrandChildren(trim(oldXml)), ddmGrandChildren(trim(newXml)), newXml.scope, newXml.scope.getURI("ddm"))
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

  private def compareNodeSeq(oldNodes: Seq[Node], newNodes: Seq[Node], newXmlScope: NamespaceBinding, scheme: String) = {
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
        </prov:file>.copy(scope = newXmlScope)
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
