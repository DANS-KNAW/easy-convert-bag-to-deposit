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
import scala.xml.{ Elem, Node, NodeSeq }

case class DdmTransformer(cfgDir: File) extends DebugEnhancedLogging {

  /** remembers transformed title from profile for dcmiMetadata */
  private var profileReports: NodeSeq = NodeSeq.Empty

  private val abrRewriteRule = AbrRewriteRule(cfgDir)
  private val reportRewriteRule = ReportRewriteRule(cfgDir)

  private val profileRuleTransformer = new RuleTransformer(reportRewriteRule)
  private val dcmiRuleTransformer = new RuleTransformer(abrRewriteRule, reportRewriteRule)
  private val ddmRuleTransformer = new RuleTransformer(new RewriteRule {
    override def transform(n: Node): Seq[Node] = n match {
      case Elem(_, "dcmiMetadata", _, _, _*) =>
        <dcmiMetadata>
          { dcmiRuleTransformer(n).nonEmptyChildren }
          { profileReports }
        </dcmiMetadata>.copy(prefix = n.prefix, attributes = n.attributes, scope = n.scope)
      case _ => n
    }
  })

  def transform(n: Node): Seq[Node] = {
    profileReports = (n \ "profile" \ "title")
      .flatMap(profileRuleTransformer)
    val ddm = ddmRuleTransformer(n)
    val titles = (ddm \\ "title").text
    if (titles.toLowerCase.matches(s".*brief[^a-z]*rapport${ reportRewriteRule.nrRegExp }.*"))
      logger.info(s"briefrapport publiser=[${ddm \ "publisher"}] rightsHolder=[${ddm \ "rightsHolder"}] titles=[$titles]")
    ddm
  }
}

