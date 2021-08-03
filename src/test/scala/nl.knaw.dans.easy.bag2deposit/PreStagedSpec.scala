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

import nl.knaw.dans.easy.bag2deposit.Fixture.{ FileSystemSupport, PreStagedSupport }
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalaj.http.HttpResponse

import java.nio.file.Paths
import scala.util.Success

class PreStagedSpec extends AnyFlatSpec with Matchers with FileSystemSupport with PreStagedSupport {
  val sampleJson: String =
    """[
      |  {
      |    "label": "stringABC",
      |    "directoryLabel": "path/to",
      |    "datasetSequenceNumber": "1",
      |    "dataFile": {
      |      "storageIdentifier": "123",
      |      "fileName": "empty.txt",
      |      "mimeType": "text/plain",
      |      "checksum": {
      |        "@type": "sha1",
      |        "@value": "da39a3ee5e6b4b0d3255bfef95601890afd80709"
      |      },
      |      "fileSize": 0
      |    }
      |  },
      |  {
      |    "label": "stringABC",
      |    "directoryLabel": "another/path/to",
      |    "datasetSequenceNumber": "1",
      |    "dataFile": {
      |      "storageIdentifier": "123",
      |      "fileName": "empty.txt",
      |      "mimeType": "text/plain",
      |      "checksum": {
      |        "@type": "sha1",
      |        "@value": "da39a3ee5e6b4b0d3255bfef95601890afd80709"
      |      },
      |      "fileSize": 0
      |    }
      |  },
      |  {
      |    "label": "stringABC",
      |    "directoryLabel": "some/path/to",
      |    "datasetSequenceNumber": "1",
      |    "dataFile": {
      |      "storageIdentifier": "123",
      |      "fileName": "something.txt",
      |      "mimeType": "text/plain",
      |      "checksum": {
      |        "@type": "sha1",
      |        "@value": "blabla"
      |      },
      |      "fileSize": 0
      |    }
      |  }
      |]""".stripMargin
  private val sampleObject = new PreStaged(
    path = Paths.get("some/path/to/something.txt"),
    fileSize = 0,
    mimeType = "text/plain",
    checksumType = "sha1",
    checksumValue = "blabla",
    storageId = "123",
  )
  private val expectedCsv =
    """path,checksum,storageId
      |some/path/to/something.txt,blabla,123
      |""".stripMargin

  "provider.get" should "return a seq" in {
    val mockedProvider = mock[PreStagedProvider]
    (mockedProvider.execute(_: String)) expects "/datasets/:persistentId/seq/1/basic-file-metas?persistentId=doi:some-doi" returning
      HttpResponse(sampleJson, 200, Map.empty)
    delegatingPreStagedProvider(mockedProvider).get("some-doi") shouldBe Success(Seq(sampleObject))
  }

  "write" should "create csv" in {
    PreStaged.write(Seq(sampleObject), testDir) shouldBe Success(())
    (testDir / "pre-staged.csv").contentAsString shouldBe expectedCsv
  }
}
