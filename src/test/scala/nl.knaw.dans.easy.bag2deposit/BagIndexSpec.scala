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

import nl.knaw.dans.easy.bag2deposit.Fixture.BagIndexSupport
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalaj.http.HttpResponse

import scala.util.{ Failure, Success }

class BagIndexSpec extends AnyFlatSpec with Matchers with BagIndexSupport {

  "getSeqLength" should "return 1" in {
    val uuid = UUID.randomUUID()
    val delegate = mock[MockBagIndex]
    (delegate.execute(_: String)) expects s"/bag-sequence?contains=$uuid" returning
      new HttpResponse[String](body = uuid.toString, code = 200, Map.empty)
    delegatingBagIndex(delegate)
      .getSeqLength(uuid) shouldBe Success(1)
  }

  it should "return 3" in {
    val uuid = UUID.randomUUID()
    val delegate = mock[MockBagIndex]
    (delegate.execute(_: String)) expects s"/bag-sequence?contains=$uuid" returning
      new HttpResponse[String](body =
        s"""c01d7876-8080-4597-81fe-9083b5463cc1
           | $uuid
           |
           |ef5d6285-c835-4199-9ae8-72cf9407b81c
           |""".stripMargin, code = 200, Map.empty)
    delegatingBagIndex(delegate)
      .getSeqLength(uuid) shouldBe Success(3)
  }

  "getURN" should "report not found" in {
    val uuid = UUID.randomUUID()
    val delegate = mock[MockBagIndex]
    (delegate.execute(_: String)) expects s"/bags/$uuid" returning
      new HttpResponse[String]("", 404, Map.empty)
    delegatingBagIndex(delegate)
      .gePIDs(uuid) shouldBe Failure(InvalidBagException(s"/bags/$uuid returned not found in bag-index"))
  }

  it should "return URN" in {
    val uuid = UUID.randomUUID()
    val delegate = mock[MockBagIndex]
    (delegate.execute(_: String)) expects s"/bags/$uuid" returning
      new HttpResponse[String](
        """<result>
          |    <bag-info>
          |        <bag-id>38cb3ff1-d59d-4560-a423-6f761b237a56</bag-id>
          |        <base-id>38cb3ff1-d59d-4560-a423-6f761b237a56</base-id>
          |        <created>2016-11-13T00:41:11.000+01:00</created>
          |        <doi>10.80270/test-28m-zann</doi>
          |        <urn>urn:nbn:nl:ui:13-z4-f8cm</urn>
          |    </bag-info>
          |</result>""".stripMargin,
        200,
        Map.empty,
      )
    delegatingBagIndex(delegate)
      .gePIDs(uuid) shouldBe Success(("urn:nbn:nl:ui:13-z4-f8cm","10.80270/test-28m-zann"))
  }

  it should "report missing URN" in {
    val uuid = UUID.randomUUID()
    val delegate = mock[MockBagIndex]
    (delegate.execute(_: String)) expects s"/bags/$uuid" returning
      new HttpResponse[String]("<x/>", 200, Map.empty)
    delegatingBagIndex(delegate)
      .gePIDs(uuid) shouldBe Failure(BagIndexException(s"$uuid: no URN in <x/>", null))
  }

  it should "report invalid XML" in {
    val uuid = UUID.randomUUID()
    val delegate = mock[MockBagIndex]
    (delegate.execute(_: String)) expects s"/bags/$uuid" returning
      new HttpResponse[String]("{}", 200, Map.empty)
    delegatingBagIndex(delegate)
      .gePIDs(uuid) should matchPattern {
      case Failure(BagIndexException(msg, _)) if msg == s"$uuid: Content is not allowed in prolog. RESPONSE: {}" =>
    }
  }

  it should "report io problem" in {
    val uuid = UUID.randomUUID()
    val delegate = mock[MockBagIndex]
    (delegate.execute(_: String)) expects s"/bags/$uuid" throwing new IOException("mocked")

    delegatingBagIndex(delegate)
      .gePIDs(uuid) should matchPattern {
      case Failure(BagIndexException(msg, _)) if msg == s"/bags/$uuid mocked" =>
    }
  }

  it should "report not expected response code" in {
    val uuid = UUID.randomUUID()
    val delegate = mock[MockBagIndex]
    (delegate.execute(_: String)) expects s"/bags/$uuid" returning
      new HttpResponse[String]("", 300, Map.empty)
    delegatingBagIndex(delegate)
      .gePIDs(uuid) shouldBe Failure(BagIndexException(s"Not expected response code from bag-index. /bags/$uuid, response: 300 - ", null))
  }
}
