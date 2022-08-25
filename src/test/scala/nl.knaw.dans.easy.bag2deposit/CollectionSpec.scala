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
import com.yourmediashelf.fedora.client.FedoraClientException
import nl.knaw.dans.easy.bag2deposit.Fixture.{ DdmSupport, FileSystemSupport, SchemaSupport }
import nl.knaw.dans.easy.bag2deposit.collections.{ Collection, FedoraProvider }
import org.apache.commons.configuration.PropertiesConfiguration
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import resource.managed

import java.io.InputStream
import scala.util.Success

class CollectionSpec extends AnyFlatSpec with DdmSupport with SchemaSupport with Matchers with FileSystemSupport with MockFactory {
  override val schema = "https://raw.githubusercontent.com/DANS-KNAW/easy-schema/eade34a3c05669d05ec8cdbeb91a085d83c6c030/lib/src/main/resources/md/2021/02/ddm.xsd"

  /** TODO make sample jumpoff's smaller and have references to not-migrated datasets
   *   Currently the samples have 286 links, 84 DOIs and 199 urn:nbn. That leaves 7 other types of links.
   *   Some links are not visible on the jumpoff pages:
   *   https://github.com/DANS-KNAW/easy-convert-bag-to-deposit/blob/07e6cce0c1f5bb673596e8cbaca37c2f5988118d/src/test/resources/sample-jumpoff/for-64608.html#L167-L171
   *   https://easy.dans.knaw.nl/ui/datasets/id/easy-dataset:64608
   * */
  private val jumpoffMocks = File("src/test/resources/sample-jumpoff")

  "getCollectionsMap" should "not stumble over <br> and combine multiple datasets into a single collection" in {
    val originalCsv =
      """naam,EASY-dataset-id,type,opmerkingen,members
        |Diachron bv,"easy-dataset:33834,easy-dataset:33976",organisation
        |""".stripMargin
    val expectedCsv =
      """name,EASY-dataset-id,type,comment,members
        |Diachron bv,"easy-dataset:33834,easy-dataset:33976",organisation,,"easy-dataset:58025,easy-dataset:33807,easy-dataset:33812,easy-dataset:51902,easy-dataset:64418,easy-dataset:34202,easy-dataset:33811,easy-dataset:33772,easy-dataset:52088,easy-dataset:33808,easy-dataset:59157,easy-dataset:52839,easy-dataset:34201,easy-dataset:53179,easy-dataset:64528,easy-dataset:64415,easy-dataset:64157,easy-dataset:64176,easy-dataset:64165,easy-dataset:33948,easy-dataset:33819,easy-dataset:33814,easy-dataset:64175,easy-dataset:33949,easy-dataset:34216,easy-dataset:33813,easy-dataset:33817,easy-dataset:50269,easy-dataset:33771,easy-dataset:64311,easy-dataset:64414,easy-dataset:33557,easy-dataset:33816,easy-dataset:64195,easy-dataset:53064,easy-dataset:33777,easy-dataset:64549,easy-dataset:64503,easy-dataset:64188,easy-dataset:64163,easy-dataset:34203,easy-dataset:64090,easy-dataset:33815,easy-dataset:64164,easy-dataset:34219,easy-dataset:33818,easy-dataset:33504,easy-dataset:51426,easy-dataset:51902,easy-dataset:26437,easy-dataset:64418,easy-dataset:113273,easy-dataset:59157,easy-dataset:52839,easy-dataset:53179,easy-dataset:64528,easy-dataset:64415,easy-dataset:64157,easy-dataset:64165,easy-dataset:57123,easy-dataset:106716,easy-dataset:106588,easy-dataset:64175,easy-dataset:51885,easy-dataset:39175,easy-dataset:64311,easy-dataset:64414,easy-dataset:64195,easy-dataset:53064,easy-dataset:64549,easy-dataset:64503,easy-dataset:64188,easy-dataset:64163,easy-dataset:64090,easy-dataset:58025,easy-dataset:64164"
        |""".stripMargin
    val mockedProvider: FedoraProvider = mock[FedoraProvider]
    expectJumpoff("easy-dataset:33834", jumpoffMocks / "for-33834.html", mockedProvider)
    expectJumpoffTxt("easy-dataset:33976", jumpoffMocks / "for-33976.html", mockedProvider)
    val cfgDir = propsFile("").parent
    val csvFile = cfgDir / "ThemathischeCollecties.csv"
    csvFile.writeText(originalCsv)

    // just sampling one of the expected tuples, the keys of all tuples are written to the updated CSV
    val sampleElem =
        <ddm:inCollection
          schemeURI="https://vocabularies.dans.knaw.nl/collections"
          valueURI="https://vocabularies.dans.knaw.nl/collections/archaeology/Diachronbv"
          subjectScheme="DANS Collection"
        >Diachron bv</ddm:inCollection>

    // precondition
    csvBackUpFiles(cfgDir) should have size 0

    // the mocked jump offs are read and parsed just once (by the first call)
    Collection.getCollectionsMap(cfgDir)(mockedProvider).get("easy-dataset:64188").head.head shouldBe sampleElem
    Collection.getCollectionsMap(cfgDir)(mockedProvider).get("easy-dataset:64188").head.head shouldBe sampleElem

    // post conditions
    val files = csvBackUpFiles(cfgDir)
    files should have size 2 // one for each call
    files.minBy(_.name).contentAsString shouldBe originalCsv
    files.maxBy(_.name).contentAsString shouldBe expectedCsv
    csvFile.contentAsString shouldBe expectedCsv
  }

  it should "return members from xhtml" in {
    val originalCsv =
      """naam,EASY-dataset-id,type,opmerkingen,members
        |"Odyssee onderzoeksprojecten",easy-dataset:34359,organisatie
        |""".stripMargin
    val expectedCsv = // note that the quotes on the first field disappear
      """name,EASY-dataset-id,type,comment,members
        |Odyssee onderzoeksprojecten,easy-dataset:34359,organisatie,,""".stripMargin
    val mockedProvider: FedoraProvider = mock[FedoraProvider]
    expectJumpoffTxt("easy-dataset:34359", jumpoffMocks / "3931-for-dataset-34359.html", mockedProvider)
    val cfgDir = propsFile("").parent
    val csvFile = cfgDir / "ThemathischeCollecties.csv"
    csvFile.writeText(originalCsv)

    Collection.getCollectionsMap(cfgDir)(mockedProvider) shouldBe a[Map[_, _]]
    csvFile.contentAsString should startWith(expectedCsv)
    csvFile.contentAsString.split("\n").last.matches(".*easy-dataset:34359.*,easy-dataset:.*") shouldBe true
  }

  it should "not stumble over verzamelpagina van verzamelpagina's" in {
    val originalCsv =
      """naam,EASY-dataset-id,type,opmerkingen,members
        |Verzamelpagina Archeologie,easy-dataset:33895,N/A,Dit is de 'verzamelpagina van verzamelpagina's': totaaloverzicht van archeologische collecties per organisatie en project
        |""".stripMargin
    val expectedCsv =
      """name,EASY-dataset-id,type,comment,members
        |Verzamelpagina Archeologie,easy-dataset:33895,N/A,Dit is de 'verzamelpagina van verzamelpagina's': totaaloverzicht van archeologische collecties per organisatie en project,"""".stripMargin
    val mockedProvider: FedoraProvider = mock[FedoraProvider]
    expectJumpoff("easy-dataset:33895", jumpoffMocks / "for-33895.html", mockedProvider)
    val cfgDir = propsFile("").parent
    val csvFile = cfgDir / "ThemathischeCollecties.csv"
    csvFile.writeText(originalCsv)

    val collectionMap = Collection.getCollectionsMap(cfgDir)(mockedProvider)
    collectionMap.values.toList.distinct.head.head.text.trim shouldBe
      "Verzamelpagina Archeologie not found in collections skos"
    csvFile.contentAsString should startWith(expectedCsv)
    csvFile.contentAsString.split("\n").last.matches(".*easy-dataset:33895.*,easy-dataset:.*") shouldBe true
  }

  it should "not stumble <br> nor over not found DOI" in {
    val originalCsv =
      """naam,EASY-dataset-id,type,opmerkingen,members
        |Oral history,"easy-dataset:64608",organisatie
        |""".stripMargin
    val expectedCsv = // note that the quotes on the second field disappear
      """name,EASY-dataset-id,type,comment,members
        |Oral history,easy-dataset:64608,organisatie,,"""".stripMargin
    val mockedProvider: FedoraProvider = mock[FedoraProvider]
    expectJumpoff("easy-dataset:64608", jumpoffMocks / "for-64608.html", mockedProvider)
    val cfgDir = propsFile("").parent
    val csvFile = cfgDir / "ThemathischeCollecties.csv"
    csvFile.writeText(originalCsv)

    Collection.getCollectionsMap(cfgDir)(mockedProvider) shouldBe a[Map[_, _]]
    csvFile.contentAsString should startWith(expectedCsv)
    csvFile.contentAsString.split("\n").last.matches(".*easy-dataset:64608.*,easy-dataset:.*") shouldBe true
    // TODO manual check: should log "ERROR not found: https://doi.org/10.17026/dans-xg5-6zwxBLABLABLA"
  }

  "FedoraProvider" should "return None if URL not configured" in {
    FedoraProvider(new PropertiesConfiguration() {
      setDelimiterParsingDisabled(true)
    }) shouldBe None
  }
  it should "return None if empty URL configured" in {
    FedoraProvider(new PropertiesConfiguration() {
      setDelimiterParsingDisabled(true)
      addProperty("fcrepo.url", "")
    }) shouldBe None
    // ConfigurationSpec shows DdmTransformer will get an empty collectionsMap in this case
  }
  it should "return Some, even if not available" in {
    FedoraProvider(new PropertiesConfiguration() {
      setDelimiterParsingDisabled(true)
      addProperty("fcrepo.url", "https://does.not.exist.dans.knaw.nl")
      addProperty("fcrepo.user", "mocked")
      addProperty("fcrepo.password", "mocked")
    }) shouldBe a[Some[_]]
    // ConfigurationSpec shows the application won't start in this case
  }

  private def csvBackUpFiles(cfgDir: File) = {
    cfgDir.list.filter(_.name.startsWith("ThemathischeCollecties-")).toArray
  }

  private def expectJumpoff(datasetId: String, file: File, mockedProvider: FedoraProvider) = {
    (mockedProvider.getJumpoff(
      _: String
    )) expects datasetId returning Success(Some("dans-jumpoff:mocked"))

    (mockedProvider.disseminateDatastream(
      _: String,
      _: String,
    )) expects("dans-jumpoff:mocked", "HTML_MU") returning managed(file.newFileInputStream) once()

    mockedProvider
  }

  private def expectJumpoffTxt(datasetId: String, file: File, fedoraProvider: FedoraProvider) = {
    val mockedInputStream = mock[InputStream]
    (mockedInputStream.read(_: Array[Byte], _: Int, _: Int)) expects(*, *, *) throwing new FedoraClientException(404, "mocked fedora stream ont found")
    mockedInputStream.close _ expects()

    (fedoraProvider.getJumpoff(
      _: String
    )) expects datasetId returning Success(Some("dans-jumpoff:mocked"))

    (fedoraProvider.disseminateDatastream(
      _: String,
      _: String,
    )) expects("dans-jumpoff:mocked", "HTML_MU") returning managed(mockedInputStream) once()

    (fedoraProvider.disseminateDatastream(
      _: String,
      _: String,
    )) expects("dans-jumpoff:mocked", "TXT_MU") returning managed(file.newFileInputStream) once()

    fedoraProvider
  }
}
