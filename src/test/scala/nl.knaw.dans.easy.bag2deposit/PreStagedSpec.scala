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
      |    "label":"3-of-4.jpg",
      |    "versionSequenceNumber":1,
      |    "prestagedFile":{
      |      "storageIdentifier":"file://17b2f03c4c8-c08fbd7eee88",
      |      "fileName":"3-of-4.jpg",
      |      "mimeType":"image/jpeg",
      |      "checksum":{
      |        "@type":"SHA-1",
      |        "@value":"1776070b9338352a5be96847187c96b30987dfd5"
      |      },
      |      "categories":[
      |
      |      ],
      |      "restrict":false,
      |      "forceReplace":false
      |    }
      |  },
      |  {
      |    "label":"brick-to-overlap-choices.png",
      |    "directoryLabel":"gf/diagrams",
      |    "versionSequenceNumber":1,
      |    "prestagedFile":{
      |      "storageIdentifier":"file://17b2f042ee9-ed12da9cbf50",
      |      "fileName":"brick-to-overlap-choices.png",
      |      "mimeType":"image/png",
      |      "checksum":{
      |        "@type":"SHA-1",
      |        "@value":"393432fcc68b5cd8b4b3bf15c5244c2631577694"
      |      },
      |      "categories":[
      |
      |      ],
      |      "restrict":false,
      |      "forceReplace":false
      |    }
      |  }
      |]""".stripMargin
  private val expectedCsv =
    """path,checksum,storageId
      |some/path/to/something.txt,blabla,123
      |""".stripMargin

  "provider.get" should "return a seq" in {
    val mockedProvider = mock[PreStagedProvider]
    (mockedProvider.execute(_: String)) expects "/datasets/:persistentId/seq/1/basic-file-metas?persistentId=doi:some-doi" returning
      HttpResponse(sampleJson, 200, Map.empty)
    val preStaged = Seq(
      new PreStaged(
        path = Paths.get("gf/diagrams/brick-to-overlap-choices.png"),
        mimeType = "image/png",
        checksumType = "SHA-1",
        checksumValue = "393432fcc68b5cd8b4b3bf15c5244c2631577694",
        storageId = "file://17b2f042ee9-ed12da9cbf50",
      ),
      new PreStaged(
        path = Paths.get("3-of-4.jpg"),
        mimeType = "image/jpeg",
        checksumType = "SHA-1",
        checksumValue = "1776070b9338352a5be96847187c96b30987dfd5",
        storageId = "file://17b2f03c4c8-c08fbd7eee88",
      ),
    )
    delegatingPreStagedProvider(mockedProvider).get("some-doi") shouldBe Success(preStaged)
  }

  "write" should "create csv" in {
    val preStaged = new PreStaged(
      path = Paths.get("some/path/to/something.txt"),
      mimeType = "text/plain",
      checksumType = "sha1",
      checksumValue = "blabla",
      storageId = "123",
    )
    PreStaged.write(Seq(preStaged), testDir) shouldBe Success(())
    (testDir / "pre-staged.csv").contentAsString shouldBe expectedCsv
  }
}
