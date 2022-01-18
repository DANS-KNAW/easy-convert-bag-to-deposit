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
import nl.knaw.dans.easy.bag2deposit.InvalidBagException
import nl.knaw.dans.easy.bag2deposit.ddm.DistinctTitlesRewriteRule.distinctTitles
import nl.knaw.dans.easy.bag2deposit.ddm.LanguageRewriteRule.logNotMappedLanguages
import nl.knaw.dans.easy.bag2deposit.ddm.ReportRewriteRule.logBriefRapportTitles
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{Failure, Success, Try}
import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Elem, Node, NodeSeq, Text}

class DdmTransformer(cfgDir: File, collectionsMap: Map[String, Seq[Elem]] = Map.empty) extends DebugEnhancedLogging {
  trace(())
  val reportRewriteRule: ReportRewriteRule = ReportRewriteRule(cfgDir)
  private val acquisitionRewriteRule = AcquisitionRewriteRule(cfgDir)
  private val relationRewriteRule = RelationRewriteRule(cfgDir)
  private val languageRewriteRule = LanguageRewriteRule(cfgDir / "languages.csv")
  private val profileArchaeologicalTitleRuleTransformer = new RuleTransformer(
    acquisitionRewriteRule,
    reportRewriteRule,
    relationRewriteRule,
  )

  private val dcmiMetadataArchaeologyRuleTransformer = new RuleTransformer(
    SplitNrRewriteRule,
    acquisitionRewriteRule,
    reportRewriteRule,
    AbrRewriteRule.temporalRewriteRule(cfgDir),
    AbrRewriteRule.subjectRewriteRule(cfgDir),
    languageRewriteRule,
    relationRewriteRule,
  )

  private def standardRuleTransformer(newDcmiNodes: NodeSeq, profileTitle: String) = new RuleTransformer(
    NewDcmiNodesRewriteRule(newDcmiNodes),
    DistinctTitlesRewriteRule(profileTitle),
    relationRewriteRule,
    languageRewriteRule,
    ProfileDateRewriteRule,
  )

  private case class ArchaeologyRewriteRule(profileTitle: String, additionalDcmiNodes: NodeSeq) extends RewriteRule {
    override def transform(node: Node): Seq[Node] = {
      node.label match {
        case "profile" =>
          <profile>
              { node.nonEmptyChildren.flatMap(ProfileDateRewriteRule) }
          </profile>.copy(prefix = node.prefix, attributes = node.attributes, scope = node.scope)
        case "dcmiMetadata" =>
          <dcmiMetadata>
              { distinctTitles(profileTitle, dcmiMetadataArchaeologyRuleTransformer(node).nonEmptyChildren) }
              { additionalDcmiNodes }
          </dcmiMetadata>.copy(prefix = node.prefix, attributes = node.attributes, scope = node.scope)
        case _ => node
      }
    }
  }

  private def unknownRightsHolder(ddm: Node) = {
    val inRole = (ddm \\ "role").text.toLowerCase.contains("rightsholder")
    if (inRole || (ddm \ "dcmiMetadata" \ "rightsHolder").nonEmpty) Seq.empty
    else <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
      .copy(prefix = ddm.scope.getPrefix("http://purl.org/dc/terms/"))
  }

  def transform(ddmIn: Node, datasetId: String): Try[Node] = {
    trace(datasetId)
    val tmp = collectionsMap.mapValues(_.size).filter(_._2>1).keys.toList.sortBy(identity)
    trace(tmp.mkString(","))
    val newDcmiNodes = missingLicense(ddmIn) ++
      collectionsMap.get(datasetId).toSeq.flatten ++
      unknownRightsHolder(ddmIn)

    val profile = ddmIn \ "profile"

    if (!(profile \ "audience").text.contains("D37000")) {
      // not archaeological
      val transformer = standardRuleTransformer(newDcmiNodes, (profile \ "title").text)
      Success(transformer(ddmIn))
    }
    else {
      // a title in the profile will not change but may produce something for dcmiMetadata
      val transformedProfile = profile.flatMap(profileArchaeologicalTitleRuleTransformer)
      val fromFirstTitle = transformedProfile.flatMap(_.nonEmptyChildren)
        .diff(profile.flatMap(_.nonEmptyChildren))
      val notConvertedFirstTitle = transformedProfile \ "title"

      // the transformation
      val ddmRuleTransformer = new RuleTransformer(ArchaeologyRewriteRule(
        profileTitle = (profile \ "title").text,
        additionalDcmiNodes = fromFirstTitle ++ newDcmiNodes
      ))
      val ddmOut = ddmRuleTransformer(ddmIn)

      // logging
      val notConvertedTitles = (ddmOut \ "dcmiMetadata" \ "title") ++ notConvertedFirstTitle
      logBriefRapportTitles(notConvertedTitles, ddmOut, datasetId)

      if ((ddmOut \\ "notImplemented").isEmpty) Success(ddmOut)
      else Failure(InvalidBagException((ddmOut \\ "notImplemented").map(_.text).mkString("; ")))
    }
  }.map { ddm =>
    logNotMappedLanguages(ddm, datasetId)
    ddm
  }

  private def missingLicense(ddm: Node): Seq[Node] = {
    val value = (ddm \\ "license").nonEmpty
    if (value) Seq.empty
    else {
      (ddm \\ "accessRights").headOption.map(_.text match {
        case "OPEN_ACCESS_FOR_REGISTERED_USERS" | "REQUEST_PERMISSION" =>
          <dcterms:license xsi:type="dcterms:URI">http://dans.knaw.nl/en/about/organisation-and-policy/legal-information/DANSLicence.pdf</dcterms:license>
        case _ => Text("")
      }).getOrElse(Seq.empty)
    }
  }
}

