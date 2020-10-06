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

import java.util.UUID

import better.files.File
import nl.knaw.dans.easy.bag2deposit.Fixture.{ AppConfigSupport, FileSystemSupport }
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{ Failure, Success }

class BagInfoSpec extends AnyFlatSpec with Matchers with AppConfigSupport with FileSystemSupport {
  val bags: File = File("src/test/resources/bags/01")

  "apply" should "succeed" in {
    val file = bags / "04e638eb-3af1-44fb-985d-36af12fccb2d" / "bag-revision-1" / "bag-info.txt"
    BagInfo(file) shouldBe a[Success[_]] // see also FactorySpec
  }
  it should "complain about invalid uuid" in {
    val file = bags / "no-uuid" / "bag-name" / "bag-info.txt"
    BagInfo(file) shouldBe Failure(InvalidBagException(
      s"Invalid UUID: $bags/no-uuid"
    ))
  }
  it should "complain about not existing bag-info.txt" in {
    val file = bags / "42c8dcbe-df51-422e-9c7e-9bd7a0b1ecc0" / "no-bag" / "bag-info.txt"
    BagInfo(file) shouldBe Failure(InvalidBagException(
      s"Unable to load the configuration from the URL file:$file"
    ))
  }
  it should "complain about missing created" in {
    val uuid = UUID.randomUUID()
    val bag = (testDir / s"0$uuid" / "bag-name").createDirectories()
    val file = (bag / "bag-info.txt").write("EASY-User-Account: user001")
    BagInfo(file) shouldBe Failure(InvalidBagException(
      s"No Bagging-Date in $file"
    ))
  }
  it should "complain about missing user account" in {
    val uuid = UUID.randomUUID()
    val bag = (testDir / s"$uuid" / "bag-name").createDirectories()
    val file = (bag / "bag-info.txt").write("Created: 2017-01-16T14:35:00.888+01:00")
    BagInfo(file) shouldBe Failure(InvalidBagException(
      s"No EASY-User-Account in $file"
    ))
  }
  it should "complain about too long uuid in version-of" in {
    val uuid = UUID.randomUUID()
    val bag = (testDir / uuid.toString / "bag-name").createDirectories()
    val file = (bag / "bag-info.txt").write(
      s"""EASY-User-Account: user001
         |Bagging-Date: 2017-01-16T14:35:00.888+01:00
         |Is-Version-Of: urn:uuid:123456789$uuid
         |""".stripMargin)
    BagInfo(file) shouldBe Failure(InvalidBagException(
      s"Invalid UUID: Is-Version-Of: urn:uuid:123456789$uuid"
    ))
  }
  it should "have no version-of" in {
    val uuid = UUID.randomUUID()
    val bag = (testDir / s"$uuid" / "bag-name").createDirectories()
    val dateTime = """2017-01-16T14:35:00.888+01:00"""
    val file = (bag / "bag-info.txt").write(
      s"""Bagging-Date: $dateTime
         |EASY-User-Account: user001
         |""".stripMargin)
    BagInfo(file) shouldBe Success(new BagInfo(
      "user001", None, dateTime, uuid, "bag-name"
    ))
  }
  it should "have a version-of" in {
    val bagUuid = UUID.randomUUID()
    val versionOfUuid = UUID.randomUUID()
    val bag = (testDir / s"$bagUuid" / "bag-name").createDirectories()
    val dateTime = """2017-01-16T14:35:00.888+01:00"""
    val file = (bag / "bag-info.txt").write(
      s"""Bagging-Date: $dateTime
         |Is-Version-Of: $versionOfUuid
         |EASY-User-Account: user001
         |""".stripMargin)
    BagInfo(file) shouldBe Success(new BagInfo(
      "user001", Some(versionOfUuid), dateTime, bagUuid, "bag-name"
    ))
  }
}
