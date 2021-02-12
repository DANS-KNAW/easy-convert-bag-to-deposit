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
package nl.knaw.dans.easy.bag2deposit.collections

import com.yourmediashelf.fedora.client.request.RiSearch
import com.yourmediashelf.fedora.client.{ FedoraClient, FedoraCredentials }
import org.apache.commons.configuration.PropertiesConfiguration
import org.apache.commons.io.IOUtils
import resource.{ ManagedResource, managed }

import java.io.InputStream
import java.net.URL
import scala.util.{ Failure, Try }

class FedoraProvider(fedoraClient: FedoraClient) {
  // dressed down copy of https://github.com/DANS-KNAW/easy-fedora-tobag

  // which is a copy of https://github.com/DANS-KNAW/easy-export-dataset/blob/6e656c6e6dad19bdea70694d63ce929ab7b0ad2b/src/main/scala/nl.knaw.dans.easy.export/FedoraProvider.scala
  // variant of https://github.com/DANS-KNAW/easy-deposit-agreement-creator/blob/e718655515ad5d597fd227bc29776c074a959f00/src/main/scala/nl/knaw/dans/easy/agreement/datafetch/Fedora.scala#L52
  def getSubordinates(datasetId: String): Try[Seq[String]] = {
    search(
      s"""
         |PREFIX dans: <http://dans.knaw.nl/ontologies/relations#>
         |SELECT ?s WHERE {?s dans:isSubordinateTo <info:fedora/$datasetId> . }
         |""".stripMargin)
      .map(_.drop(1).map(_.split("/").last))
  }

  private def search(query: String): Try[Seq[String]] = {
    val riSearch = new RiSearch(query).lang("sparql").format("csv")
    managed(riSearch.execute(fedoraClient))
      .flatMap(response => managed(response.getEntityInputStream))
      .map(is => new String(IOUtils.toByteArray(is)).split("\n").toSeq)
      .tried
      .recoverWith {
        case t: Throwable =>
          Failure(new Exception(s"$this, query '$query' failed, cause: ${ t.getMessage }", t))
      }
  }

  def disseminateDatastream(objectId: String, streamId: String): ManagedResource[InputStream] = {
    managed(FedoraClient.getDatastreamDissemination(objectId, streamId).execute(fedoraClient))
      .flatMap(response => managed(response.getEntityInputStream))
  }
}
object FedoraProvider {
  def apply(properties: PropertiesConfiguration): Option[FedoraProvider] = {
    Option(properties.getString("fcrepo.url")).map(url =>
      new FedoraProvider(new FedoraClient(new FedoraCredentials(
        new URL(url),
        properties.getString("fcrepo.user"),
        properties.getString("fcrepo.password"),
      )))
    )
  }
}
