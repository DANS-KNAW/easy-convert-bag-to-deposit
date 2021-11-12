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

import better.files.File
import nl.knaw.dans.easy.bag2deposit.parseCsv
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.csv.CSVFormat

import scala.xml._
import scala.xml.transform.RewriteRule

case class RelationRewriteRule(cfgDir: File) extends RewriteRule with DebugEnhancedLogging {

  private val easyRef = "https://easy.dans.knaw.nl/ui/datasets/id/easy-dataset:"
  private val urnRef = "urn:nbn:nl:ui:13"
  private val relations = List(
    "relation",
    "conformsTo",
    "hasFormat",
    "hasPart",
    "hasVersion",
    "isFormatOf",
    "isPartOf",
    "isReferencedBy",
    "isReplacedBy",
    "isRequiredBy",
    "isVersionOf",
    "references",
    "replaces",
    "requires"
  )

  override def transform(node: Node): Seq[Node] = {
    if (!relations.contains(node.label))
      node
    else {
      val txt = node.text.trim
      val href = node.attribute("href").toSeq.flatten.text.trim
      val doi = if (href.startsWith(easyRef)) easyRefToDoi(href)
                else if (txt.startsWith(easyRef)) easyRefToDoi(txt)
                     else if (href.contains(urnRef)) urnRefToDoi(href)
                          else if (txt.contains(urnRef)) urnRefToDoi(href)
                               else ""
      lazy val otherAttributes = node.attributes.remove("href").remove("scheme")
      lazy val doiAttributes = {
        Attribute(null, "scheme", "id-type:DOI",
          Attribute(null, "href", doi, otherAttributes)
        )
      }

      if (href.isEmpty && txt.isEmpty) Seq.empty
      else (node, doi, node.prefix) match {
        case (elem: Elem, "", "ddm") if href.isEmpty && txt.matches("https?://.*") =>
          elem.copy(attributes = Attribute(null, "href", txt, otherAttributes))
        case (elem: Elem, "", _) if txt.isEmpty =>
          elem.copy(child = Text(href))
        case (elem: Elem, "", _) =>
          elem
        case (elem: Elem, _, "ddm") if txt.startsWith(easyRef) || txt.contains(urnRef) || txt.isEmpty =>
          elem.copy(attributes = doiAttributes, child = Text(doi))
        case (elem: Elem, _, "ddm") =>
          elem.copy(attributes = doiAttributes)
        case (elem: Elem, _, _) =>
          elem.copy(child = Text(doi))
      }
    }
  }

  private def urnRefToDoi(value: String) = ???

  private def easyRefToDoi(value: String) = {
    val easyDataset = value.split('/').last
    datasetDoiMap.get(easyDataset).map { doi =>
      logger.warn(s"$value replaced with DOI")
      doi
    }.getOrElse(value)
  }

  private val csvFormat = CSVFormat.RFC4180
    .withHeader("dataset", "doi")
    .withDelimiter(',')
    .withRecordSeparator('\n')

  val datasetDoiMap: Map[String, String] = {
    parseCsv(cfgDir / "dataset-doi.csv", nrOfHeaderLines = 1, csvFormat)
      .map(record => record.get("dataset") -> ("https://doi.org/" + record.get("doi"))).toMap
  }
}

