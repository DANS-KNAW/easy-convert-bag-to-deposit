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

import better.files.File
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.xml.transform.{ RewriteRule, RuleTransformer }
import scala.xml.{ Node, NodeSeq }

case class DdmTransformer(cfgDir: File) extends DebugEnhancedLogging {

  /** remembers transformed title from profile for dcmiMetadata */
  private var profileReportNumber: NodeSeq = NodeSeq.Empty

  private val reportRewriteRule = ReportRewriteRule(cfgDir)
  private val profileRuleTransformer = new RuleTransformer(reportRewriteRule)
  private val dcmiMetadataRuleTransformer = new RuleTransformer(
    reportRewriteRule,
    AbrRewriteRule(cfgDir / "ABR-period.csv", "temporal", "ddm:temporal"),
    AbrRewriteRule(cfgDir / "ABR-complex.csv", "subject", "ddm:subject"),
  )
  private val ddmRuleTransformer = new RuleTransformer(new RewriteRule {
    override def transform(n: Node): Seq[Node] = {
      if (n.label != "dcmiMetadata") n
      else <dcmiMetadata>
             { dcmiMetadataRuleTransformer(n).nonEmptyChildren }
             { profileReportNumber }
           </dcmiMetadata>.copy(prefix = n.prefix, attributes = n.attributes, scope = n.scope)
    }
  })

  val nrTailRegexp = s"${ reportRewriteRule.nrRegExp }${ reportRewriteRule.trailer }"
  def transform(ddmIn: Node): Seq[Node] = {
    val firstTitle = (ddmIn \ "profile" \ "title").flatMap(profileRuleTransformer)

    profileReportNumber = firstTitle.filter(_.label == "reportNumber")
    val ddmOut = ddmRuleTransformer(ddmIn)

    val titles = (ddmOut \ "dcmiMetadata" \ "title") +: firstTitle.filter(_ => profileReportNumber.isEmpty)
    titles.foreach { node =>
      val title = node.text
      if (title.toLowerCase.matches(s"brief[^a-z]*rapport$nrTailRegexp"))
        logger.info(s"briefrapport rightsHolder=[${ ddmOut \ "rightsHolder" }] publisher=[${ ddmOut \ "publisher" }] titles=[$title]")
      else if (title.toLowerCase.matches(s".*(notitie|rapport|bericht|publicat).*$nrTailRegexp"))
             logger.info(s"potential report number: $title") // TODO logs too much
    }
    ddmOut
  }
}

