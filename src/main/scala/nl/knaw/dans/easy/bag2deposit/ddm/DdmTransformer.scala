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
import nl.knaw.dans.easy.bag2deposit.InvalidBagException
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Failure, Success, Try }
import scala.xml.transform.{ RewriteRule, RuleTransformer }
import scala.xml.{ Node, NodeSeq }

case class DdmTransformer(cfgDir: File) extends DebugEnhancedLogging {

  private val reportRewriteRule = ReportRewriteRule(cfgDir)
  private val profileRuleTransformer = new RuleTransformer(reportRewriteRule)
  private val dcmiMetadataRuleTransformer = new RuleTransformer(
    reportRewriteRule,
    AbrRewriteRule(cfgDir / "ABR-period.csv", "temporal", "ddm:temporal"),
    AbrRewriteRule(cfgDir / "ABR-complex.csv", "subject", "ddm:subject"),
  )

  private case class DdmRewriteRule(reportNumberFromFirstTitle: NodeSeq) extends RewriteRule {
    override def transform(n: Node): Seq[Node] = {
      if (n.label != "dcmiMetadata") n
      else <dcmiMetadata>
             { dcmiMetadataRuleTransformer(n).nonEmptyChildren }
             { reportNumberFromFirstTitle }
           </dcmiMetadata>.copy(prefix = n.prefix, attributes = n.attributes, scope = n.scope)
    }
  }

  def transform(ddmIn: Node): Try[Node] = {

    // the single title may become a title and/or reportNumber
    val transformedFirstTitle = (ddmIn \ "profile" \ "title").flatMap(profileRuleTransformer)
    val reportNumberFromFirstTitle = transformedFirstTitle.filter(_.label == "reportNumber")
    val notConvertedFirstTitle = transformedFirstTitle.filter(_ => reportNumberFromFirstTitle.isEmpty)

    // the transformation
    val ddmRuleTransformer = new RuleTransformer(DdmRewriteRule(reportNumberFromFirstTitle))
    val ddmOut = ddmRuleTransformer(ddmIn)

    // logging and error handling
    val notConvertedTitles = (ddmOut \ "dcmiMetadata" \ "title") ++ notConvertedFirstTitle
    logBriefRapportTitles(notConvertedTitles, ddmOut)
    val problems = ddmOut \\ "notImplemented" // fail slow trick
    if (problems.nonEmpty)
      Failure(InvalidBagException(problems.map(_.text).mkString("; ")))
    else ddmOut.headOption.map(Success(_))
      .getOrElse(Failure(InvalidBagException("DDM transformation returned empty sequence")))
  }

  private def logBriefRapportTitles(notConvertedTitles: NodeSeq, ddmOut: Node): Unit = {
    // these titles need a more complex transformation or manual fix before the final export
    notConvertedTitles.foreach { node =>
      val title = node.text
      if (title.toLowerCase.matches(s"brief[^a-z]*rapport${ reportRewriteRule.nrTailRegexp } }"))
        logger.info(s"briefrapport rightsHolder=[${ ddmOut \ "rightsHolder" }] publisher=[${ ddmOut \ "publisher" }] titles=[$title]")
    }
  }
}

