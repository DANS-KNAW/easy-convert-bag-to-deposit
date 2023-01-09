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
  private val remarksMap: Map[String, RemarksCategory] = {
    val file: File = cfgDir / "remarks-mapping.csv"
    if (!file.exists)
      Map.empty
    else parseCsv(
        file = file,
        nrOfHeaderLines = 1,
        format = RFC4180.withHeader("dataset_id", "category"),
      ).map(r =>
        r.get("dataset_id") -> Try(RemarksCategory.withName(r.get("category"))).getOrElse {
          logger.warn(s"${ r.get("dataset_id") } has an invalid remark category, using a plain description")
          RemarksCategory.description
        }
      ).toMap
  }

  def additionalDcmi(emd: File, datasetId: String, fromVault: Boolean): Try[NodeSeq] = {
    val cat = remarksMap.getOrElse(datasetId, RemarksCategory.description)

    def convert(remark: NodeSeq): NodeSeq = {
      if (remark.isEmpty || remark.text == "")
        NodeSeq.Empty
      else {
        cat match {
          case RemarksCategory.access => <dct:accessRights>{ remark.text }</dct:accessRights>
          case RemarksCategory.citation => <dct:bibliographicCitation>{ remark.text }</dct:bibliographicCitation>
          case RemarksCategory.contact |
               RemarksCategory.contributor |
               RemarksCategory.funder => <ddm:description descriptionType="Other">{ remark.text }</ddm:description>
          case RemarksCategory.copyright => <dct:rightsHolder>{ remark.text }</dct:rightsHolder>
          case RemarksCategory.description => <dct:description>{ remark.text }</dct:description>
          case RemarksCategory.files => <ddm:description descriptionType="TechnicalInfo">{ remark.text }</ddm:description>
          case RemarksCategory.ignore => NodeSeq.Empty
          case RemarksCategory.provenance => <dct:provenance>{ remark.text }</dct:provenance>
          case RemarksCategory.relation => <dc:relation>{ remark.text }</dc:relation>
          case RemarksCategory.collectiondate => <dc:date>{ remark.text }</dc:date>
        }
      }
    }

    if (fromVault || cat == RemarksCategory.ignore)
      Success(NodeSeq.Empty)
    else for {
      emd <- Try(XML.loadFile(emd.toJava))
      remark = emd \\ "remark"
      _ = if(remark.nonEmpty && ! remarksMap.contains(datasetId))
        logger.warn(s"$datasetId has a remark field but no mapping, using a plain description")
    } yield convert(remark)
  }
}
protected object RemarksCategory extends Enumeration {
  type RemarksCategory = Value
  val access, citation, contact, contributor, copyright, description, files, funder, ignore, provenance, relation, collectiondate = Value
}
