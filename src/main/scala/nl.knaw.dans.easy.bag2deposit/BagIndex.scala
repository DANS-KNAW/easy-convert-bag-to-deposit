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
package nl.knaw.dans.easy.bag2deposit

import java.io.IOException
import java.net.URI
import java.util.UUID

import better.files.StringExtensions
import scalaj.http.{ Http, HttpResponse }

import scala.util.{ Failure, Try }
import scala.xml.XML

case class BagIndexException(msg: String, cause: Throwable) extends IOException(msg, cause)

case class BagIndex(bagIndexUri: URI) {

  def getURN(uuid: UUID): Try[String] = for {
    response <- find(uuid)
    xml <- parse(response, uuid)
    nodes = xml \\ "urn"
    urn = nodes.theSeq.headOption.map(_.text)
      .getOrElse(throw BagIndexException(s"$uuid: no URN in $response", null))
  } yield urn

  private def parse(response: String, uuid: UUID) = Try {
    XML.load(response.inputStream)
  }.recoverWith {
    case t: Throwable => Failure(BagIndexException(s"$uuid: ${t.getMessage} RESPONSE: $response", t))
  }

  private def find(uuid: UUID): Try[String] = Try {
    execute(uuid)
  }.recoverWith {
    case t: Throwable => Failure(BagIndexException(s"$uuid " + t.getMessage, t))
  }.map {
    case response if response.code == 404 => throw InvalidBagException(s"$uuid not found in bag-index")
    case response if response.code == 200 => response.body
    case response => throw BagIndexException(
      s"Not expected response code from bag-index. $uuid, response: ${ response.code } - ${ response.body }",
      null,
    )
  }

  protected[BagIndex] def execute(uuid: UUID): HttpResponse[String] = {
    Http(bagIndexUri.resolve(s"/bags/$uuid").toString)
      .header("Accept", "text/xml")
      .asString
  }
}
