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
import com.sun.jersey.api.client.ClientHandlerException
import nl.knaw.dans.easy.bag2deposit.Fixture.{ DdmSupport, FileSystemSupport, SchemaSupport }
import nl.knaw.dans.easy.bag2deposit.collections.Collections.getCollectionsMap
import nl.knaw.dans.easy.bag2deposit.collections.{ Collections, FedoraProvider }
import nl.knaw.dans.lib.error.TryExtensions
import org.apache.commons.configuration.PropertiesConfiguration
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import resource.managed

import java.net.UnknownHostException
import scala.util.{ Failure, Success, Try }

class CollectionsSpec extends AnyFlatSpec with DdmSupport with SchemaSupport with Matchers with FileSystemSupport with MockFactory {
  override val schema = "https://raw.githubusercontent.com/DANS-KNAW/easy-schema/eade34a3c05669d05ec8cdbeb91a085d83c6c030/lib/src/main/resources/md/2021/02/ddm.xsd"

  private val cfgDir = File("src/main/assembly/dist/cfg")

  private def getIgnoreOrThrow[T](result: Try[T]): T = result
    .recoverWith { case e: UnknownHostException =>
      assume(false, "UnknownHostException")
      fail(e.getMessage)
    }.unsafeGetOrThrow

  private val jumpoffMocks = File("src/test/resources/sample-jumpoff")

  "collectionDatasetIdToInCollection" should "produce valid elements" in {
    val tuples = Collections.collectionDatasetIdToInCollection(cfgDir).toMap
    tuples.values
      .filter(_.label == "notImplemented")
      .map(_.text) shouldBe Seq(
      "Oogst van Malta � onderzoeksprogramma not found in collections skos",
      "Verzamelpagina Archeologie not found in collections skos",
    )
    tuples.size shouldBe (cfgDir / "ThemathischeCollecties.csv")
      .lines.size + 10

    // just sampling some of the resulting tuples:
    tuples("easy-dataset:32660") shouldBe tuples("easy-dataset:33600")
    tuples("easy-dataset:32660") shouldBe
      <ddm:inCollection
        schemeURI="http://easy.dans.knaw.nl/vocabularies/collecties"
        valueURI="http://easy.dans.knaw.nl/vocabularies/collecties#ADC"
        subjectScheme="DANS Collection"
      >ADC</ddm:inCollection>
    tuples("easy-dataset:34359") shouldBe
      <ddm:inCollection
        schemeURI="http://easy.dans.knaw.nl/vocabularies/collecties"
        valueURI="http://easy.dans.knaw.nl/vocabularies/collecties#Odysseeonderzoeksprojecten"
        subjectScheme="DANS Collection"
      >Odyssee onderzoeksprojecten</ddm:inCollection>

    assume(schemaIsAvailable)
    validate(ddm(title = "blabla", audience = "D37000", dcmi =
      <ddm:dcmiMetadata>
        { tuples.values.toSeq.filter(_.label != "notImplemented") }
      </ddm:dcmiMetadata>
    )) shouldBe Success(())
  }

  "memberDatasetIdToInCollection" should "return members from xhtml" in {
    val fedoraProvider: FedoraProvider = mock[FedoraProvider]
    expectJumpoff(jumpoffMocks / "3931-for-dataset-34359.html", fedoraProvider)
    val mockedJumpoffMembers = List("easy-dataset:34099", "easy-dataset:57698", "easy-dataset:57517", "easy-dataset:50715", "easy-dataset:46315", "easy-dataset:50635", "easy-dataset:62503", "easy-dataset:31688", "easy-dataset:48388", "easy-dataset:57281", "easy-dataset:50610", "easy-dataset:62773", "easy-dataset:41884", "easy-dataset:68647", "easy-dataset:54459", "easy-dataset:50636", "easy-dataset:54529", "easy-dataset:61129", "easy-dataset:55947", "easy-dataset:47464", "easy-dataset:60949", "easy-dataset:55302", "easy-dataset:62505", "easy-dataset:50711")

    val tuples = Try(
      Collections.memberDatasetIdToInCollection(Seq("easy-dataset:mocked" -> <inCollection>mocked</inCollection>), fedoraProvider)
    ).unsafeGetOrThrow

    tuples.values.toList.distinct shouldBe Seq(<inCollection>mocked</inCollection>)
    tuples.keys.toList.sortBy(identity) shouldBe mockedJumpoffMembers.sortBy(identity)
  }

  it should "return members from html containing <br>" in {
    val fedoraProvider: FedoraProvider = mock[FedoraProvider]
    expectJumpoff(jumpoffMocks / "for-dataset-64608.html", fedoraProvider)
    val mockedJumpoffMembers = List("easy-dataset:113728", "easy-dataset:113729", "easy-dataset:113730", "easy-dataset:113731", "easy-dataset:113732", "easy-dataset:113733", "easy-dataset:113734", "easy-dataset:113735", "easy-dataset:113736", "easy-dataset:113737", "easy-dataset:113738", "easy-dataset:113749", "easy-dataset:113750", "easy-dataset:113751", "easy-dataset:113752", "easy-dataset:113753", "easy-dataset:113754", "easy-dataset:113755", "easy-dataset:113756", "easy-dataset:113757", "easy-dataset:113758", "easy-dataset:113759", "easy-dataset:113760", "easy-dataset:113761", "easy-dataset:113762", "easy-dataset:113763", "easy-dataset:113764", "easy-dataset:113765", "easy-dataset:113766", "easy-dataset:113777", "easy-dataset:113778", "easy-dataset:113779", "easy-dataset:113782", "easy-dataset:113784", "easy-dataset:190096", "easy-dataset:190097", "easy-dataset:190098", "easy-dataset:190099", "easy-dataset:190100", "easy-dataset:190101", "easy-dataset:190102", "easy-dataset:190103", "easy-dataset:190104", "easy-dataset:190105", "easy-dataset:190106", "easy-dataset:190107", "easy-dataset:190108", "easy-dataset:190109", "easy-dataset:190110", "easy-dataset:190111", "easy-dataset:190112", "easy-dataset:190113", "easy-dataset:190114", "easy-dataset:190117", "easy-dataset:190118", "easy-dataset:190119", "easy-dataset:190120", "easy-dataset:190121", "easy-dataset:190122", "easy-dataset:190123", "easy-dataset:190124", "easy-dataset:190126", "easy-dataset:190127", "easy-dataset:190128", "easy-dataset:190129", "easy-dataset:190130", "easy-dataset:190132", "easy-dataset:190133", "easy-dataset:190134", "easy-dataset:190135", "easy-dataset:190136", "easy-dataset:190137", "easy-dataset:190138", "easy-dataset:190139")

    val tuples = Try(
      Collections.memberDatasetIdToInCollection(Seq("easy-dataset:mocked" -> <inCollection>mocked</inCollection>), fedoraProvider)
    ).unsafeGetOrThrow

    tuples.values.toList.distinct shouldBe Seq(<inCollection>mocked</inCollection>)
    tuples.keys.toList.sortBy(identity) shouldBe mockedJumpoffMembers.sortBy(identity)
  }

  private def expectJumpoff(file: File, fedoraProvider: FedoraProvider) = {
    (fedoraProvider.getSubordinates(
      _: String
    )) expects "easy-dataset:mocked" returning Success(Seq("dans-jumpoff:mocked", "easy-file:123"))
    (fedoraProvider.disseminateDatastream(
      _: String,
      _: String,
    )) expects("dans-jumpoff:mocked", "HTML_MU") returning managed(file.newFileInputStream) once()
  }

  "FedoraProvider" should "return None if URL not configured" in {
    FedoraProvider(new PropertiesConfiguration() {
      setDelimiterParsingDisabled(true)
    }) shouldBe None
  }
  it should "return Node if empty URL configured" in {
    FedoraProvider(new PropertiesConfiguration() {
      setDelimiterParsingDisabled(true)
      addProperty("fcrepo.url", "")
    }) shouldBe None
  }
  it should "return Some, even if not available" in {
    FedoraProvider(new PropertiesConfiguration() {
      setDelimiterParsingDisabled(true)
      load("src/main/assembly/dist/cfg/application.properties")
    }) shouldBe a[Some[_]]
  }
  "getCollectionsMap" should "cause an exception when called to create a Configuration object" in {
    Try(getCollectionsMap(File("src/test/resources/debug-config"), new PropertiesConfiguration() {
      setDelimiterParsingDisabled(true)
      addProperty("fcrepo.url", "https://does.not.exist.dans.knaw.nl")
      addProperty("fcrepo.user", "mocked")
      addProperty("fcrepo.password", "mocked")
    })) should matchPattern {
      case Failure(e) if e.getCause.isInstanceOf[ClientHandlerException] &&
        e.getCause.getCause.isInstanceOf[UnknownHostException] =>
    }
  }
}
