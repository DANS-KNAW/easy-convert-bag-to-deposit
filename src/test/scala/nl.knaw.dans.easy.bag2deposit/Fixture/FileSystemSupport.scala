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
package nl.knaw.dans.easy.bag2deposit.Fixture

import better.files.File
import better.files.File.currentWorkingDirectory
import org.scalatest.BeforeAndAfterEach
import org.scalatest.enablers.Existence
import org.scalatest.flatspec.AnyFlatSpec

trait FileSystemSupport extends BeforeAndAfterEach {
  this: AnyFlatSpec =>

  implicit def existenceOfFile[FILE <: better.files.File]: Existence[FILE] = _.exists

  override def beforeEach(): Unit = {
    super.beforeEach()

    if (testDir.exists) testDir.delete()
    testDir.createDirectories()
  }

  lazy val testDir: File = currentWorkingDirectory / "target" / "test" / getClass.getSimpleName

  def propsFile(fedoraUrl: String): File = {
    val distSrc = File("src/main/assembly/dist")
    distSrc.copyToDirectory(testDir)
    (testDir / "dist" / "cfg" / "application.properties").writeText(
      (distSrc / "cfg" / "application.properties").contentAsString
        .replace("http://localhost:20120/", fedoraUrl)
    )
  }
}
