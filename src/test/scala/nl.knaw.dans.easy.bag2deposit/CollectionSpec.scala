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

import better.files.File
import com.yourmediashelf.fedora.client.FedoraClientException
import nl.knaw.dans.easy.bag2deposit.Fixture.{ DdmSupport, FileSystemSupport, SchemaSupport }
import nl.knaw.dans.easy.bag2deposit.collections.{ Collection, FedoraProvider }
import org.apache.commons.configuration.PropertiesConfiguration
import org.joda.time.{ DateTime, DateTimeUtils }
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import resource.managed

import java.io.InputStream
import java.net.UnknownHostException
import scala.util.{ Failure, Success, Try }

class CollectionSpec extends AnyFlatSpec with DdmSupport with SchemaSupport with Matchers with FileSystemSupport with MockFactory {
  override val schema = "https://raw.githubusercontent.com/DANS-KNAW/easy-schema/eade34a3c05669d05ec8cdbeb91a085d83c6c030/lib/src/main/resources/md/2021/02/ddm.xsd"
  private val jumpoffMocks = File("src/test/resources/sample-jumpoff")

  "getCollectionsMap" should "create valid elements for each collection dataset" in {
    val originalCsv =
      """naam,EASY-dataset-id,type,opmerkingen,members
        |Diachron bv,"easy-dataset:33834,easy-dataset:33976",organisatie
        |""".stripMargin
    val expectedNewCsv =
      """name,EASY-dataset-id,type,comment,members
        |Diachron bv,"easy-dataset:33834,easy-dataset:33976",organisatie,,"easy-dataset:58025,easy-dataset:33807,easy-dataset:33812,easy-dataset:51902,easy-dataset:64418,easy-dataset:34202,easy-dataset:33811,easy-dataset:33772,easy-dataset:52088,easy-dataset:33808,easy-dataset:59157,easy-dataset:52839,easy-dataset:34201,easy-dataset:53179,easy-dataset:64528,easy-dataset:64415,easy-dataset:64157,easy-dataset:64176,easy-dataset:64165,easy-dataset:33948,easy-dataset:33819,easy-dataset:33814,easy-dataset:64175,easy-dataset:33949,easy-dataset:34216,easy-dataset:33813,easy-dataset:33817,easy-dataset:50269,easy-dataset:33771,easy-dataset:64311,easy-dataset:64414,easy-dataset:33557,easy-dataset:33816,easy-dataset:64195,easy-dataset:53064,easy-dataset:33777,easy-dataset:64549,easy-dataset:64503,easy-dataset:64188,easy-dataset:64163,easy-dataset:34203,easy-dataset:64090,easy-dataset:33815,easy-dataset:64164,easy-dataset:34219,easy-dataset:33818,easy-dataset:33504,easy-dataset:51426,easy-dataset:51902,easy-dataset:26437,easy-dataset:64418,easy-dataset:113273,easy-dataset:59157,easy-dataset:52839,easy-dataset:53179,easy-dataset:64528,easy-dataset:64415,easy-dataset:64157,easy-dataset:64165,easy-dataset:57123,easy-dataset:106716,easy-dataset:106588,easy-dataset:64175,easy-dataset:51885,easy-dataset:39175,easy-dataset:64311,easy-dataset:64414,easy-dataset:64195,easy-dataset:53064,easy-dataset:64549,easy-dataset:64503,easy-dataset:64188,easy-dataset:64163,easy-dataset:64090,easy-dataset:58025,easy-dataset:64164"
        |""".stripMargin
    val mockedProvider: FedoraProvider = mock[FedoraProvider]
    expectJumpoff("easy-dataset:33834", jumpoffMocks / "for-33834.html", mockedProvider)
    expectJumpoff("easy-dataset:33976", jumpoffMocks / "for-33976.html", mockedProvider)
    val cfgDir = propsFile("").parent
    val csvFile = cfgDir / "ThemathischeCollecties.csv"
    csvFile.writeText(originalCsv)

    // just sampling one of the expected tuples, the keys of all tuples are written to the updated CSV
    val sampleTuple = "easy-dataset:64188" ->
        <ddm:inCollection
          schemeURI="http://easy.dans.knaw.nl/vocabularies/collecties"
          valueURI="http://easy.dans.knaw.nl/vocabularies/collecties#Diachronbv"
          subjectScheme="DANS Collection"
        >Diachron bv</ddm:inCollection>

    // precondition
    csvBackUpFiles(cfgDir) should have size 0

    // the mocked jump offs are read and parsed just once (by the first call)
    Collection.getCollectionsMap(cfgDir, Some(mockedProvider)) should contain(sampleTuple)
    Collection.getCollectionsMap(cfgDir, Some(mockedProvider)) should contain(sampleTuple)

    // post conditions
    val files = csvBackUpFiles(cfgDir)
    files should have size 2 // one for each call
    files.minBy(_.name).contentAsString shouldBe originalCsv
    files.maxBy(_.name).contentAsString shouldBe expectedNewCsv
    csvFile.contentAsString shouldBe expectedNewCsv
  }

  it should "not duplicate CSV when fedora is not configured" in {
    val cfgDir = propsFile("").parent
    Collection.getCollectionsMap(cfgDir, None) shouldBe a[Map[_,_]]
    csvBackUpFiles(cfgDir) should have size 0
  }

  "memberDatasetIdToInCollection" should "return members from xhtml" in {
    val mockedProvider: FedoraProvider = mock[FedoraProvider]
    //    expectJumpoff("easy-dataset:mocked1", jumpoffMocks / "3931-for-dataset-34359.html", mockedProvider)
    val mockedJumpoffMembers = List("easy-dataset:34099", "easy-dataset:57698", "easy-dataset:57517", "easy-dataset:50715", "easy-dataset:46315", "easy-dataset:50635", "easy-dataset:62503", "easy-dataset:31688", "easy-dataset:48388", "easy-dataset:57281", "easy-dataset:50610", "easy-dataset:62773", "easy-dataset:41884", "easy-dataset:68647", "easy-dataset:54459", "easy-dataset:50636", "easy-dataset:54529", "easy-dataset:61129", "easy-dataset:55947", "easy-dataset:47464", "easy-dataset:60949", "easy-dataset:55302", "easy-dataset:62505", "easy-dataset:50711")
    // TODO
  }

  it should "return members from html containing <br>" in {
    //    val fedoraProvider = expectJumpoffTxt("easy-dataset:mocked2", jumpoffMocks / "for-dataset-64608.html")
    val mockedJumpoffMembers = List("easy-dataset:113728", "easy-dataset:113729", "easy-dataset:113730", "easy-dataset:113731", "easy-dataset:113732", "easy-dataset:113733", "easy-dataset:113734", "easy-dataset:113735", "easy-dataset:113736", "easy-dataset:113737", "easy-dataset:113738", "easy-dataset:113749", "easy-dataset:113750", "easy-dataset:113751", "easy-dataset:113752", "easy-dataset:113753", "easy-dataset:113754", "easy-dataset:113755", "easy-dataset:113757", "easy-dataset:113758", "easy-dataset:113759", "easy-dataset:113760", "easy-dataset:113761", "easy-dataset:113762", "easy-dataset:113763", "easy-dataset:113764", "easy-dataset:113765", "easy-dataset:113766", "easy-dataset:113777", "easy-dataset:113778", "easy-dataset:113782", "easy-dataset:113784", "easy-dataset:190096", "easy-dataset:190097", "easy-dataset:190098", "easy-dataset:190099", "easy-dataset:190100", "easy-dataset:190101", "easy-dataset:190102", "easy-dataset:190103", "easy-dataset:190104", "easy-dataset:190105", "easy-dataset:190106", "easy-dataset:190107", "easy-dataset:190108", "easy-dataset:190109", "easy-dataset:190110", "easy-dataset:190111", "easy-dataset:190112", "easy-dataset:190113", "easy-dataset:190114", "easy-dataset:190117", "easy-dataset:190118", "easy-dataset:190119", "easy-dataset:190120", "easy-dataset:190121", "easy-dataset:190122", "easy-dataset:190123", "easy-dataset:190124", "easy-dataset:190126", "easy-dataset:190127", "easy-dataset:190128", "easy-dataset:190129", "easy-dataset:190130", "easy-dataset:190132", "easy-dataset:190133", "easy-dataset:190134", "easy-dataset:190135", "easy-dataset:190136", "easy-dataset:190137", "easy-dataset:190138", "easy-dataset:190139")

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

  private def getIgnoreOrThrow[T](tried: Try[T]): T = {
    tried match {
      case Success(t) => t
      case Failure(e: UnknownHostException) =>
        assume(false)
        throw e
      case Failure(e) => throw e
    }
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

  private def expectJumpoffTxt(datasetId: String, file: File) = {
    val fedoraProvider: FedoraProvider = mock[FedoraProvider]

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
