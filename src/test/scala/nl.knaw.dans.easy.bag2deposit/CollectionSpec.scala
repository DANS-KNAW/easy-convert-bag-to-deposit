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

import nl.knaw.dans.easy.bag2deposit.Fixture.{ AppConfigSupport, DdmSupport, FileSystemSupport, SchemaSupport }
import nl.knaw.dans.easy.bag2deposit.collections.Collection
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Try
import scala.xml.Node

class CollectionSpec extends AnyFlatSpec with DdmSupport with SchemaSupport with Matchers with FileSystemSupport with MockFactory with AppConfigSupport{
  override val schema = "https://raw.githubusercontent.com/DANS-KNAW/easy-schema/eade34a3c05669d05ec8cdbeb91a085d83c6c030/lib/src/main/resources/md/2021/02/ddm.xsd"

  "getSeriesNode" should "return a description Node" in {
    val loadFoXmlResult =
      <foxml:digitalObject xsi:schemaLocation="info:fedora/fedora-system:def/foxml# http://www.fedora.info/definitions/1/0/foxml1-1.xsd" PID="easy-dataset:17" VERSION="1.1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:foxml="info:fedora/fedora-system:def/foxml#">
        <foxml:datastream VERSIONABLE="false" CONTROL_GROUP="X" STATE="A" ID="EMD">
          <foxml:datastreamVersion SIZE="4820" MIMETYPE="text/xml" CREATED="2021-06-04T12:06:56.477Z" LABEL="Administrative metadata for this dataset" ID="AMD.0">
            <foxml:xmlContent>
              <emd:easymetadata xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dct="http://purl.org/dc/terms/" xmlns:eas="http://easy.dans.knaw.nl/easy/easymetadata/eas/" xmlns:emd="http://easy.dans.knaw.nl/easy/easymetadata/" emd:version="0.1">
                <emd:title>
                  <dc:title>Getuigen Verhalen, Burgerarbeiders in kamp Vught, interview 05</dc:title>
                </emd:title>
                <emd:description>
                  <dc:description>Herinneringen van inwoners uit Vught en Cromvoirt aan het bestaan van een concentratiekamp in hun directe omgeving.</dc:description>
                </emd:description>
              </emd:easymetadata>
            </foxml:xmlContent>
          </foxml:datastreamVersion>
        </foxml:datastream>
      </foxml:digitalObject>

    val expected = <ddm:description descryptionType="SeriesInformation">
                      Herinneringen van inwoners uit Vught en Cromvoirt aan het bestaan van een concentratiekamp in hun directe omgeving.
                    </ddm:description>
    val fedoraProvider = mock[MockFedoraProvider]
    (fedoraProvider.loadFoXml _).expects("easy-dataset:33834").returning(Try(loadFoXmlResult)).once()
    val series : Node = Collection.getSeriesNode(List("easy-dataset:33834","easy-dataset:33976"), Some(fedoraProvider), Set("easy-dataset:33834"))
    series.text.trim shouldBe expected.text.trim
  }

  "getCollectionsMap" should "not stumble over <br> and combine multiple datasets into a single collection" in {
    val originalCsv =
      """naam,EASY-dataset-id,type,opmerkingen,members
        |Diachron bv,"easy-dataset:33834,easy-dataset:33976",organisation,,"easy-dataset:64188"
        |""".stripMargin
    val cfgDir = propsFile("").parent / "archaeology"
    val csvFile = cfgDir / "ThemathischeCollecties.csv"
    csvFile.writeText(originalCsv)

    // just sampling one of the expected tuples, the keys of all tuples are written to the updated CSV
    val sampleElem =
        <ddm:inCollection
          schemeURI="https://vocabularies.dans.knaw.nl/collections"
          valueURI="https://vocabularies.dans.knaw.nl/collections/archaeology/Diachronbv"
          subjectScheme="DANS Collection"
        >Diachron bv</ddm:inCollection>


    Collection.getCollectionsMap(cfgDir, Some(null)).get("easy-dataset:64188").head.head shouldBe sampleElem
  }
}
