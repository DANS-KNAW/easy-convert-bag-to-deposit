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

import nl.knaw.dans.easy.bag2deposit.Fixture.FileSystemSupport
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Paths
import scala.util.Success

class PreStagedSpec extends AnyFlatSpec with Matchers with FileSystemSupport {
  private val sampleJson: String =
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
      |        "@type": "md5",
      |        "@value": "d41d8cd98f00b204e9800998ecf8427e"
      |      },
      |      "fileSize": 0
      |    }
      |  }
      |]""".stripMargin
  private val sampleObject = new PreStaged(
    Paths.get("path/to/empty.txt"),
    0,
    "text/plain",
    "md5",
    "d41d8cd98f00b204e9800998ecf8427e",
    "123",
  )
  private val expectedCsv =
    """path,checksum,storageId
      |path/to/empty.txt,d41d8cd98f00b204e9800998ecf8427e,123
      |""".stripMargin

  "apply" should "parse json" in {
    PreStaged(sampleJson) shouldBe Success(Seq(sampleObject))
  }

  "write" should "create csv" in {
    PreStaged.write(Seq(sampleObject), testDir) shouldBe Success(())
    (testDir / "pre-staged.csv").contentAsString shouldBe expectedCsv
  }
}
