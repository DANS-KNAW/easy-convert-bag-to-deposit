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

import java.io.IOException
import java.util.UUID

import nl.knaw.dans.easy.bag2deposit.fixture.BagIndexSupport
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{ Failure, Success }

class BagIndexSpec extends AnyFlatSpec with Matchers with BagIndexSupport {

  "getURN" should "report not found" in {
    val uuid = UUID.randomUUID()
    mockBagIndexRespondsWith(body = "", code = 404)
      .getURN(uuid) shouldBe Failure(InvalidBagException(s"$uuid not found in bag-index"))
  }

  it should "return URN" in {
    val uuid = UUID.randomUUID()
    mockBagIndexRespondsWith(body =
      """<result>
        |    <bag-info>
        |        <bag-id>38cb3ff1-d59d-4560-a423-6f761b237a56</bag-id>
        |        <base-id>38cb3ff1-d59d-4560-a423-6f761b237a56</base-id>
        |        <created>2016-11-13T00:41:11.000+01:00</created>
        |        <doi>10.80270/test-28m-zann</doi>
        |        <urn>urn:nbn:nl:ui:13-z4-f8cm</urn>
        |    </bag-info>
        |</result>""".stripMargin, code = 200)
      .getURN(uuid) shouldBe Success("urn:nbn:nl:ui:13-z4-f8cm")
  }

  it should "report missing URN" in {
    val uuid = UUID.randomUUID()
    mockBagIndexRespondsWith(body =
      "<x/>".stripMargin, code = 200)
      .getURN(uuid) shouldBe Failure(BagIndexException(s"$uuid: no URN in <x/>", null))
  }

  it should "report invalid XML" in {
    val uuid = UUID.randomUUID()
    mockBagIndexRespondsWith(body =
      "{}".stripMargin, code = 200)
      .getURN(uuid) should matchPattern {
      case Failure(BagIndexException(msg, _)) if msg == s"$uuid: Content is not allowed in prolog. RESPONSE: {}" =>
    }
  }

  it should "report io problem" in {
    val uuid = UUID.randomUUID()
    mockBagIndexThrows(new IOException("mocked"))
      .getURN(uuid) should matchPattern {
      case Failure(BagIndexException(msg, _)) if msg == s"$uuid mocked" =>
    }
  }

  it should "" in {
    val uuid = UUID.randomUUID()
    mockBagIndexRespondsWith(body = "", code = 300)
      .getURN(uuid) shouldBe Failure(BagIndexException(s"Not expected response code from bag-index. $uuid, response: 300 - ", null))
  }
}
