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

import java.net.URI
import java.util.UUID

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalaj.http.HttpResponse

import scala.util.Failure

class BagIndexSpec extends AnyFlatSpec with Matchers {

  "get" should "report not found" in {
    val uuid = UUID.randomUUID()
    BagIndexRespondsWith(body = "", code = 404)
      .get(uuid) shouldBe Failure(InvalidBagException(s"$uuid not found in bag-index"))
  }

//  it should "" in {
//    val uuid = UUID.randomUUID()
//    BagIndexRespondsWith(body = "", code = 123)
//      .get(uuid) shouldBe Failure(BagIndexException(s"Not expected response code from bag-index. $uuid, response: 123 - ",null))
//  }

  private def BagIndexRespondsWith(body: String, code: Int) = {
    new BagIndex(null) {
      override def execute(uuid: UUID): HttpResponse[String] = {
        new HttpResponse[String](body, code = code, headers = Map.empty)
      }
    }
  }
}
