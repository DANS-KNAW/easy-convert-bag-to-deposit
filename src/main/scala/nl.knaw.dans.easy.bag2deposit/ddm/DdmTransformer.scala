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
import nl.knaw.dans.easy.bag2deposit.ddm.LanguageRewriteRule.logNotMappedLanguages
import nl.knaw.dans.easy.bag2deposit.ddm.ReportRewriteRule.logBriefRapportTitles
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Failure, Success, Try }
import scala.xml.transform.{ RewriteRule, RuleTransformer }
import scala.xml.{ Elem, Node, NodeSeq }

case class DdmTransformer(cfgDir: File, collectionsMap: Map[String, Elem] = Map.empty) extends DebugEnhancedLogging {

  val reportRewriteRule: ReportRewriteRule = ReportRewriteRule(cfgDir)
  private val acquisitionRewriteRule: AcquisitionRewriteRule = AcquisitionRewriteRule(cfgDir)
  private val profileTitleRuleTransformer = new RuleTransformer(
    acquisitionRewriteRule,
    reportRewriteRule,
  )
  private val archaeologyRuleTransformer = new RuleTransformer(
    acquisitionRewriteRule,
    reportRewriteRule,
    AbrRewriteRule.temporalRewriteRule(cfgDir),
    AbrRewriteRule.subjectRewriteRule(cfgDir),
    LanguageRewriteRule(cfgDir / "languages.csv"),
  )
  private val standardRuleTransformer = new RuleTransformer(
    LanguageRewriteRule(cfgDir / "languages.csv"),
  )

  private case class ArchaeologyRewriteRule(additionalElements: NodeSeq) extends RewriteRule {
    override def transform(n: Node): Seq[Node] = {
      if (n.label != "dcmiMetadata") n
      else <dcmiMetadata>
             { additionalElements }
             { archaeologyRuleTransformer(n).nonEmptyChildren }
           </dcmiMetadata>.copy(prefix = n.prefix, attributes = n.attributes, scope = n.scope)
    }
  }

  def transform(ddmIn: Node, datasetId: String): Try[Node] = {
    if (!(ddmIn \ "profile" \ "audience").text.contains("D37000")) {
      // not archaeological
      // TODO additional element inCollection
      Success(standardRuleTransformer(ddmIn))
    }
    else {
      val inCollection = collectionsMap.get(datasetId).toSeq

      // a title in the profile will not change but may produce something for dcmiMetadata
      val originalProfile = ddmIn \ "profile"
      val transformedProfile = originalProfile.flatMap(profileTitleRuleTransformer)
      val fromFirstTitle = transformedProfile.flatMap(_.nonEmptyChildren)
        .diff(originalProfile.flatMap(_.nonEmptyChildren))
      val notConvertedFirstTitle = transformedProfile \ "title"

      // the transformation
      val ddmRuleTransformer = new RuleTransformer(ArchaeologyRewriteRule(fromFirstTitle ++ inCollection))
      val ddmOut = ddmRuleTransformer(ddmIn)

      // logging
      val notConvertedTitles = (ddmOut \ "dcmiMetadata" \ "title") ++ notConvertedFirstTitle
      logBriefRapportTitles(notConvertedTitles, ddmOut, datasetId)

      // error handling (fail slow on not found ABR period/complex)
      val problems = ddmOut \\ "notImplemented"
      if (problems.nonEmpty)
        Failure(InvalidBagException(problems.map(_.text).mkString("; ")))
      else ddmOut.headOption.map(Success(_))
        .getOrElse(Failure(InvalidBagException("DDM transformation returned empty sequence")))
    }
  }.map { ddm =>
    logNotMappedLanguages(ddm, datasetId)
    ddm
  }
}

