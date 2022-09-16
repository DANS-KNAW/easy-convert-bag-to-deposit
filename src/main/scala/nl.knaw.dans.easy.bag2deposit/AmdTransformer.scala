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
import org.apache.commons.csv.CSVFormat.RFC4180
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

import java.nio.charset.Charset
import scala.util.{ Failure, Success, Try }
import scala.xml.transform.{ RewriteRule, RuleTransformer }
import scala.xml.{ Elem, Node, NodeSeq, Text }

class AmdTransformer(csvFile: File) {
  private val userMap = if (!csvFile.exists || csvFile.isEmpty)
                          Map[String, String]()
                        else parseCsv(
                          csvFile,
                          nrOfHeaderLines = 1,
                          format = RFC4180.withHeader("old", "new"),
                        ).map(r => r.get("old") -> r.get("new")).toMap

  private val userRewriteRule: RewriteRule = new RewriteRule {
    override def transform(node: Node): Seq[Node] = {
      if (!Seq("depositorId", "signerId").contains(node.label)) node
      else userMap
        .get(node.text).map(id => <id>{ id }</id>.copy(label = node.label))
        .getOrElse(node)
    }
  }

  /** replace oldest publication date to get the same citation date on dataverse as in easy */
  private def citationDateRewriteRule(dateCreated: String, oldest: String) = new RewriteRule() {
    override def transform(node: Node): Seq[Node] = node match {
      case e: Elem if node.label == "changeDate" && node.text.trim == oldest =>
        e.copy(child = new Text(dateCreated))
      case other => other
    }
  }

  // The default charset is determined during virtual-machine startup and typically
  // depends upon the locale and charset of the underlying operating system.
  implicit val charset: Charset = Charset.forName("UTF-8")

  def transform(xmlIn: Node, ddmCreated: NodeSeq): Try[Node] = {
    if (ddmCreated.isEmpty)
      return Failure(InvalidBagException("no date created found in DDM"))
    val ddmCreatedString = ddmCreated.head.text
    val yearCreated = yearOf(ddmCreatedString)
    val dateTimeCreated =new DateTime(ddmCreatedString)
      .toString(ISODateTimeFormat.dateTime())
    val changedToPublished = (xmlIn \\ "stateChangeDate")
      .filter(n => (n \ "toState").text.trim == "PUBLISHED")
    val oldestPublished = if (changedToPublished.isEmpty) NodeSeq.Empty
    else changedToPublished.minBy(n => (n \ "changeDate").text.trim)
    val oldestDate = (oldestPublished \ "changeDate").text.trim
    val transformer =
      if (yearCreated.length >= 4 && yearOf(oldestDate) == yearCreated)
        new RuleTransformer(userRewriteRule)
      else
        new RuleTransformer(
          userRewriteRule,
          citationDateRewriteRule(dateTimeCreated, oldestDate),
        )
    transformer
      .transform(xmlIn).headOption.map(Success(_))
      .getOrElse(Failure(new Exception("transformer did not return an AMD")))
  }

  private def yearOf(text: String) = {
    if (text.length>=4)
      text.substring(0, 4)
    else ""
  }
}
