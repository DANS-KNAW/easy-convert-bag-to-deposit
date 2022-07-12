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
package nl.knaw.dans.easy.bag2deposit.collections

import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import scalaj.http.Http

import scala.util.{ Success, Try }

case class Resolver(dataverseApiKey: String) extends DebugEnhancedLogging{

  def getDatasetId(id: String): Try[Option[String]] = {
    id.slice(0, 3) match {
      case "eas" => Success(Some(id))
      case "urn" => resolve(s"http://www.persistent-identifier.nl/?identifier=$id")
      case "10." => resolve(s"https://doi.org/$id")
      case id =>
        logger.error(s"not expected type of ID for a dataset: $id")
        Success(None)
    }
  }

  def resolveIfMigrated(supposedEasyURL: String): String = {
    if (!supposedEasyURL.contains(".nl/citation?")) supposedEasyURL
    else {
      val url = supposedEasyURL.replace("citation", "api/datasets/:persistentId/versions/1.0/metadata/citation") +
        "&"
      trace(url)
      val request = Http(url = url).copy(headers = Seq(
        ("Content-Type","application/json"),
        ("X-Dataverse-key",dataverseApiKey)
      ))
      Try(request.asString).map {
        case response if response.code == 200 =>
          response.body.replaceAll(".*easy-dataset:","easy-dataset:").replaceAll("\".*","")
        case response =>
          logger.error(s"Not expected response code from '$url' ${ response.code } - ${ response.body }")
          supposedEasyURL
      }.doIfFailure { case e =>
        logger.error(s"could not resolve $url: $e") // no stack trace, usually a timeout
      }.getOrElse(supposedEasyURL)
    }
  }

  private def resolve(url: String) = {
    trace(url)
    Try(Http(url).asString).flatMap {
      case response if response.code == 404 =>
        logger.error(s"not found: $url")
        Success(None)
      case response if response.code == 302 =>
        Success(Some(response
          .header("Location")
          .map(resolveIfMigrated)
          .map(_.replaceAll(".*/", "").replace("%3A", ":"))
          .getOrElse(throw new Exception(s"no location header returned by $url - ${ response.body }"))
        ))
      case response =>
        logger.error(s"Not expected response code from '$url' ${ response.code } - ${ response.body }")
        Success(None)
    }
  }
}
