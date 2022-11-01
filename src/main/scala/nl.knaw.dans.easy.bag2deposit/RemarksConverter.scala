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

import better.files.File
import nl.knaw.dans.easy.bag2deposit.RemarksCategory.RemarksCategory
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.csv.CSVFormat.RFC4180

import scala.util.{ Success, Try }
import scala.xml.{ NodeSeq, XML }

class RemarksConverter(cfgDir: File) extends DebugEnhancedLogging {
  private val file: File = cfgDir / "remarks-mapping.csv"
  private val remarksMap: Map[String, RemarksCategory] = {
    if (!file.exists)
      Map.empty
    else parseCsv(
        file = file,
        nrOfHeaderLines = 1,
        format = RFC4180.withHeader("dataset_id", "category"),
      ).map(r =>
        r.get("dataset_id") -> RemarksCategory.withName(r.get("category"))
      ).toMap
  }

  def additionDcmi(emd: File, datasetId: String, fromVault: Boolean): Try[NodeSeq] = {
    val cat = remarksMap.getOrElse(datasetId, {
      if (!fromVault) logger.warn(s"$datasetId has a remarks field but no mapping")
      RemarksCategory.description
    })

    def convert(remarks: NodeSeq): NodeSeq = {
      cat match {
        case RemarksCategory.access => <dct:accessRights>{ remarks.text }</dct:accessRights>
        case RemarksCategory.citation => <dct:bibliographicCitation>{ remarks.text }</dct:bibliographicCitation>
        case RemarksCategory.contact => <dc:contributor type="ContactPerson">{ remarks.text }</dc:contributor>
        case RemarksCategory.copyright => <dct:rightsHolder>{ remarks.text }</dct:rightsHolder>
        case RemarksCategory.description => <dct:description>{ remarks.text }</dct:description>
        case RemarksCategory.files => <dct:description type="TechnicalInfo">{ remarks.text }</dct:description>
        case RemarksCategory.ignore => NodeSeq.Empty
        case RemarksCategory.funder => <ddm:funder>{ remarks.text }</ddm:funder>
        case RemarksCategory.provenance => <dct:provenance>{ remarks.text }</dct:provenance>
        case RemarksCategory.relation => <dc:relation>{ remarks.text }</dc:relation>
        case RemarksCategory.collectiondate => <ddm:datesOfCollection>{ remarks.text }</ddm:datesOfCollection>
      }
    }

    if (fromVault || cat == RemarksCategory.ignore)
      Success(NodeSeq.Empty)
    else for {
      emd <- Try(XML.loadFile(emd.toJava))
      remarks = emd \ "remarks"
    } yield convert(remarks)
  }
}
protected object RemarksCategory extends Enumeration {
  type RemarksCategory = Value
  val access, citation, contact, copyright, description, files, funder, ignore, provenance, relation, collectiondate = Value
}
