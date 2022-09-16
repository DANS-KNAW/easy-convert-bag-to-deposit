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
import org.apache.commons.lang.StringUtils.{ isBlank, isNotBlank }

import scala.util.{ Failure, Success, Try }
import scala.xml._
import scala.xml.transform.{ RewriteRule, RuleTransformer }

class DdmTransformer(cfgDir: File, target: String, collectionsMap: Map[String, Seq[Elem]] = Map.empty) extends DebugEnhancedLogging {
  trace(())
  private val archaeologyCfgDir: File = cfgDir / "archaeology"
  lazy val reportRewriteRule: ReportRewriteRule = ReportRewriteRule(archaeologyCfgDir)
  private lazy val acquisitionRewriteRule = AcquisitionRewriteRule(archaeologyCfgDir)
  private val relationRewriteRule = RelationRewriteRule(cfgDir)
  private val languageRewriteRule = LanguageRewriteRule(cfgDir / "languages.csv")

  private lazy val profileToNewForDcmiTransformer = new RuleTransformer(
    acquisitionRewriteRule,
    reportRewriteRule,
  )

  private def archaeologyDcmiRuleTransformer(newDcmiNodes: NodeSeq) = new RuleTransformer(
    SplitNrRewriteRule,
    acquisitionRewriteRule,
    reportRewriteRule,
    AbrRewriteRule.temporalRewriteRule(archaeologyCfgDir),
    AbrRewriteRule.subjectRewriteRule(archaeologyCfgDir),
    ZeroPosRewriteRule,
    DatesOfCollectionRewriteRule(newDcmiNodes),
    languageRewriteRule,
    DropFunderRoleRewriteRule,
    relationRewriteRule,
  )

  private def standardRuleTransformer(newDcmiNodes: NodeSeq, profileTitle: String) = new RuleTransformer(
    NewDcmiNodesRewriteRule(newDcmiNodes),
    DistinctTitlesRewriteRule(profileTitle),
    relationRewriteRule,
    ZeroPosRewriteRule,
    DatesOfCollectionRewriteRule(newDcmiNodes),
    languageRewriteRule,
    DropFunderRoleRewriteRule,
    ProfileRewriteRule,
  )

  private case class ArchaeologyRewriteRule(profileTitle: String, additionalDcmiNodes: NodeSeq) extends RewriteRule {
    // defined local to have all creators of RuleTransformer next to one another
    override def transform(node: Node): Seq[Node] = {
      node.label match {
        case "profile" => ProfileRewriteRule(node)
        case "dcmiMetadata" =>
          <dcmiMetadata>
              { distinctTitles(profileTitle, archaeologyDcmiRuleTransformer(additionalDcmiNodes)(node).nonEmptyChildren) }
              { additionalDcmiNodes }
          </dcmiMetadata>.copy(prefix = node.prefix, attributes = node.attributes, scope = node.scope)
        case _ => node
      }
    }
  }

  private def funders(ddm: Node) = {
    (ddm \\ "contributorDetails")
      .filter(n => (n \\ "role").text == "Funder")
      .map ( node =>
        <ddm:funding>
          <ddm:funderName>{ toContributorName(node) }</ddm:funderName>
        </ddm:funding>
      )
  }

  private def toContributorName(node: Node) = {

    val title = (node \\ "title").text.trim
    val initials = (node \\ "initials").text.trim
    val prefix = (node \\ "prefix").text.trim
    val surname = (node \\ "surname").text.trim
    val organization = (node \\ "organization" \ "name").text.trim

    // copy of https://github.com/DANS-KNAW/easy-app/blob/455417523183a511f1d82cab27aaea87438e0257/lib/easy-business/src/main/java/nl/knaw/dans/easy/domain/dataset/CreatorFormatter.java#L44-L57
    val hasPersonalEntries = isNotBlank(title) || isNotBlank(prefix) || isNotBlank(initials) || isNotBlank(surname)

    (if (isNotBlank(surname)) surname + ", "
    else "") + (if (isNotBlank(title)) title + " "
    else "") + (if (isNotBlank(initials)) initials
    else "") + (if (isNotBlank(prefix)) " " + prefix
    else "") + (if (isBlank(organization)) ""
    else if (hasPersonalEntries) " (" + organization + ")"
    else organization)
  }

  private def unknownRightsHolder(ddm: Node) = {
    val inRole = (ddm \\ "role").text.toLowerCase.contains("rightsholder")
    if (inRole || (ddm \ "dcmiMetadata" \ "rightsHolder").nonEmpty) Seq.empty
    else <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
      .copy(prefix = ddm.scope.getPrefix("http://purl.org/dc/terms/"))
  }

  def transform(ddmIn: Node, datasetId: String): Try[Node] = {
    trace(datasetId)
    val newDcmiNodes = missingLicense(ddmIn) ++
      datesOfCollection(ddmIn) ++
      collectionsMap.get(datasetId).toSeq.flatten ++
      unknownRightsHolder(ddmIn) ++
      funders(ddmIn)

    val originalProfile = ddmIn \ "profile"

    if (target != "archaeology") {
      val transformer = standardRuleTransformer(newDcmiNodes, (originalProfile \ "title").text)
      Success(transformer(ddmIn))
    }
    else {
      // a title in the profile will not change but may produce something for dcmiMetadata
      val possiblyNewForDcmi = originalProfile.flatMap(profileToNewForDcmiTransformer)
      val newTitlesForDcmi = possiblyNewForDcmi.flatMap(_.nonEmptyChildren)
        .diff(originalProfile.flatMap(_.nonEmptyChildren))
      val notConvertedFirstTitle = possiblyNewForDcmi \ "title"

      // the transformation
      val ddmRuleTransformer = new RuleTransformer(ArchaeologyRewriteRule(
        profileTitle = (originalProfile \ "title").text,
        additionalDcmiNodes = newTitlesForDcmi ++ newDcmiNodes
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

  // doesn't preserve white space so don't use for serialization
  private val printer = new PrettyPrinter(160, 2)

  private def extractDate(dates: NodeSeq, containing: String) = {
    dates.filter(_.text.contains(containing)).text.trim.replaceAll(" .*", "")
  }

  private def datesOfCollection(ddm: Node): Seq[Node] = {
    val dates = (ddm \\ "date").filter(DatesOfCollectionRewriteRule.participatingDate)
    dates.size match {
      case 0 => Seq.empty
      case 1 if dates.text.matches(".*start.*") =>
        Seq(<ddm:datesOfCollection>{s"${extractDate(dates, "-")}/"}</ddm:datesOfCollection>)
      case 1 =>
        Seq(<ddm:datesOfCollection>{s"/${extractDate(dates, "-")}"}</ddm:datesOfCollection>)
      case 2 =>
        val start = extractDate(dates, "start")
        val end = extractDate(dates, "eind")
        Seq(<ddm:datesOfCollection>{s"$start/$end"}</ddm:datesOfCollection>)
      case _ =>
        logger.warn(s"Assembling datesOfCollection not implemented for ${dates.map(printer.format(_)).mkString("")}")
        Seq.empty
    }
  }
}

