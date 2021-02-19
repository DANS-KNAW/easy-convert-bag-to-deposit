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
import nl.knaw.dans.easy.bag2deposit.Fixture.FileSystemSupport
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.UnknownHostException
import scala.util.{ Failure, Try }

class ConfigurationSpec extends AnyFlatSpec with FileSystemSupport with Matchers {
  "constructor" should "create empty collectionsMap for ddmTransformer when fedora is not configured" in {
    distDir(fedoraUrl = "")
    Configuration(home = testDir / "dist").ddmTransformer
      .collectionsMap shouldBe Map.empty
  }

  it should "fail when fedora is configured but not available" in {
    distDir(fedoraUrl = "https://does.not.exist.dans.knaw.nl")

    Try(Configuration(home = testDir / "dist")) should matchPattern {
      case Failure(e) if e.getCause.isInstanceOf[ClientHandlerException] &&
        e.getCause.getCause.isInstanceOf[UnknownHostException] &&
        e.getCause.getCause.getMessage.contains("does.not.exist.dans.knaw.nl") =>
    }
  }

  private def distDir(fedoraUrl: String) = {
    val distSrc = File("src/main/assembly/dist")
    distSrc.copyToDirectory(testDir)
    (testDir / "dist" / "cfg" / "application.properties").writeText(
      (distSrc / "cfg" / "application.properties").contentAsString
        .replace("http://localhost:20120/", fedoraUrl)
    )
  }
}
