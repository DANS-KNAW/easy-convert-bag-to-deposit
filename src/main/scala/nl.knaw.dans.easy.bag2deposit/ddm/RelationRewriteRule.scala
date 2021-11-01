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

import scala.xml.{ Attribute, Elem, Node, Null }
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

  def isEasyDatasetReference(node: Node): Boolean = {
    val attr = node.attributes
    node.prefix ==  "ddm" && relations.contains(node.label) &&
      (
        attr.get("href").mkString("").startsWith(easyRef) ||
          node.text.startsWith(easyRef)
        )
  }

  override def transform(n: Node): Seq[Node] =  {
    if (!isEasyDatasetReference(n) ) n
    else {
      val easyHref = n.attribute("href").get.head.text
      val easyDataset = easyHref.split('/').last
      val doiHref = datasetDoiMap.getOrElse(easyDataset, easyHref) //TODO warning
      n.asInstanceOf[Elem] % Attribute(null, "href", doiHref, Attribute(null, "scheme", "id-type:DOI", Null))
    }
  }

  private val nrOfHeaderLines = 1
  private val csvFormat = CSVFormat.RFC4180
      .withHeader("dataset", "doi")
      .withDelimiter(',')
      .withRecordSeparator('\n')


  val datasetDoiMap: Map[String, String] = {
    parseCsv(cfgDir / "dataset-doi.csv", nrOfHeaderLines, csvFormat)
      .map(record => record.get("dataset") -> ("https://doi.org/" + record.get("doi"))).toMap
  }
}

