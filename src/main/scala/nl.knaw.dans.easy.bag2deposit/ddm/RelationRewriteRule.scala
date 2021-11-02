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
      lazy val otherAttributes = node.attributes.remove("href").remove("scheme")
      (node, href.startsWith(easyRef), href.isEmpty, txt.startsWith(easyRef), txt.isEmpty) match {
        case (_, _, true, _, true) =>
          Seq.empty
        case (elem, false, false, false, false) =>
          elem
        case (elem: Elem, _, true, false, false) if txt.matches("https?://.*") =>
          elem.copy(attributes = Attribute(null, "href", toDoi(txt), otherAttributes))
        case (elem: Elem, true, _, false, false) =>
          elem.copy(attributes = doiAttributes(href, otherAttributes))
        case (elem: Elem, _, true, true, _) =>
          elem.copy(
            attributes = doiAttributes(txt, otherAttributes),
            child = Text(toDoi(txt))
          )
        case (elem: Elem, true,_, _, true) =>
          elem.copy(
            attributes = doiAttributes(href, otherAttributes),
            child = Text(toDoi(href))
          )
        case (elem: Elem, false, false, _, true) =>
          elem.copy(child = Text(href))
        case _ => node
      }
    }
  }

  private def doiAttributes(href: String, next: MetaData) = {
    Attribute(null, "scheme", "id-type:DOI",
      Attribute(null, "href", toDoi(href), next)
    )
  }

  private def toDoi(value: String) = {
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

