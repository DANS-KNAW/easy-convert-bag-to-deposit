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
import nl.knaw.dans.easy.bag2deposit.Fixture.FileSystemSupport
import nl.knaw.dans.easy.bag2deposit.collections.Collections.getCollectionsMap
import nl.knaw.dans.easy.bag2deposit.collections.{ Collections, FedoraProvider }
import nl.knaw.dans.lib.error.TryExtensions
import org.apache.commons.configuration.PropertiesConfiguration
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.xml.sax.SAXParseException
import resource.managed

import java.net.UnknownHostException
import scala.util.{ Failure, Success, Try }
import scala.xml.XML

class CollectionsSpec extends AnyFlatSpec with Matchers with FileSystemSupport with MockFactory {
  private val cfgDir = File("src/main/assembly/dist/cfg")

  private def getIgnoreOrThrow[T](result: Try[T]): T = result
    .recoverWith { case e: UnknownHostException =>
      assume(false, "UnknownHostException")
      fail(e.getMessage)
    }.unsafeGetOrThrow

  private val jumpoffMocks = File("src/test/resources/sample-jumpoff")
  private val mockedJumpoffMembers = List("easy-dataset:34099", "easy-dataset:57698", "easy-dataset:57517", "easy-dataset:50715", "easy-dataset:46315", "easy-dataset:50635", "easy-dataset:62503", "easy-dataset:31688", "easy-dataset:48388", "easy-dataset:57281", "easy-dataset:50610", "easy-dataset:62773", "easy-dataset:41884", "easy-dataset:68647", "easy-dataset:54459", "easy-dataset:50636", "easy-dataset:54529", "easy-dataset:61129", "easy-dataset:55947", "easy-dataset:47464", "easy-dataset:60949", "easy-dataset:55302", "easy-dataset:62505", "easy-dataset:50711")

  "members" should "find dataset IDs" in {
    // works fine for archaeology, but:
    // "kamp Amersfoort" is not xhtml
    // and "Data Nederlands Onderwijsonderzoek" is a collection of collections
    // "gereviewde datasets" in "economics and business" is organised through relations
    // ...
    getIgnoreOrThrow(
      Collections.members(XML.loadFile((jumpoffMocks / "3931-for-dataset-34359.html").toJava))
    ).sortBy(identity) shouldBe mockedJumpoffMembers.sortBy(identity)
  }

  "membersOf" should "have trouble non-xhtml" in {
    // just emulating part of membersOf is less elaborate than testing with memberDatasetIdToInCollection
    // TODO perhaps use https://github.com/ruippeixotog/scala-scraper/blob/master/README.md
    //  as mentioned on https://stackoverflow.com/questions/1695902/scala-and-html-parsing
    Try(XML.loadFile((jumpoffMocks / "for-dataset-64608.html").toJava)) should matchPattern {
      case Failure(e: SAXParseException) if e.getMessage.contains("</br>") =>
    }
  }

  "collectionDatasetIdToInCollection" should "" in {
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
  }

  "memberDatasetIdToInCollection" should "" in {
    val fedoraProvider: FedoraProvider = mock[FedoraProvider]
    (fedoraProvider.getSubordinates(
      _: String
    )) expects "easy-dataset:mocked" returning Success(Seq("dans-jumpoff:mocked", "easy-file:123"))
    (fedoraProvider.disseminateDatastream(
      _: String,
      _: String,
    )) expects("dans-jumpoff:mocked", "HTML_MU") returning managed(
      (jumpoffMocks / "3931-for-dataset-34359.html").newFileInputStream
    ) once()
    val collectionDatasetIdToInCollection = Seq("easy-dataset:mocked" -> <inCollection/>)

    val tuples = getIgnoreOrThrow(Try(
      Collections.memberDatasetIdToInCollection(collectionDatasetIdToInCollection, fedoraProvider)
    )).toMap

    tuples.values.toList.distinct shouldBe Seq(<inCollection/>)
    tuples.keys.toList.sortBy(identity) shouldBe mockedJumpoffMembers.sortBy(identity)
  }

  "FedoraProvider" should "return None if URL not configured" in {
    FedoraProvider(new PropertiesConfiguration() {
      setDelimiterParsingDisabled(true)
    }) shouldBe None
  }
  it should "return Some, even if not available" in {
    FedoraProvider(new PropertiesConfiguration() {
      setDelimiterParsingDisabled(true)
      load("src/main/assembly/dist/cfg/application.properties")
    }) shouldBe a[Some[_]]
  }
  "getCollectionsMap" should "" in {
    // TODO manual check: logging should contain "Connection refused"
    getCollectionsMap(File("src/test/resources/debug-config"), new PropertiesConfiguration() {
      setDelimiterParsingDisabled(true)
      load((cfgDir / "application.properties").toJava)
    }) shouldBe Map.empty
  }
}
