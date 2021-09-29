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

import java.util.UUID
import scala.xml.transform.RewriteRule
import scala.xml.{ Elem, Node }

case class AcquisitionRewriteRule(cfgDir: File) extends RewriteRule with DebugEnhancedLogging {

  val titleMap: Map[String, Seq[UUID]] = {
    val methods: Map[String, UUID] = parseCsv(cfgDir / "acquisitionMethods.csv", 1)
      .map(r =>
        r.get(0) -> UUID.fromString(r.get(1))
      ).toMap

    parseCsv(cfgDir / "acquisitionTitleMap.csv", 1)
      .map(r =>
        r.get(1) -> r.get(0).split("; *").toSeq
          .map(s => methods.getOrElse(s, {
            logger.warn(s"$s not found, will skip this for $r")
            null
          })).filter(Option(_).nonEmpty)
      )
      .toMap
  }

  override def transform(n: Node): Seq[Node] = {
    if (n.label != "title" && n.label != "alternative") n
    else titleMap.get(n.text) // is an Option[Seq[UUID]]
      .map(_.map(toMethod(n.text)))
      .getOrElse(n) // TODO log if edit distance under some threshold? issues: spelling / word order
  }

  private def toMethod(titleValue: String)(uuid: UUID): Elem = {
    <ddm:acquisitionMethod
      schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/554ca1ec-3ed8-42d3-ae4b-47bcb848b238"
      valueURI={ s"https://data.cultureelerfgoed.nl/term/id/abr/$uuid" }
      subjectScheme="ABR verwervingswijzen"
    >{ titleValue }</ddm:acquisitionMethod>
  }
}
