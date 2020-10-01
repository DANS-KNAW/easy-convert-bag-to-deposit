/**
 * Copyright (C) 2018 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
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

import nl.knaw.dans.easy.bag2deposit.BagIndexInfo._
import org.joda.time.DateTime
import org.joda.time.DateTimeZone.UTC
import org.joda.time.format.ISODateTimeFormat
import org.json4s.JsonAST.{ JObject, JValue }
import org.json4s.ext.{ JodaTimeSerializers, UUIDSerializer }
import org.json4s.native.JsonMethods
import org.json4s.{ DefaultFormats, Extraction, Formats, JsonInput }

import scala.util.{ Failure, Success, Try }

case class BagIndexInfo(bagId: UUID = UUID.randomUUID(),
                        baseId: UUID = UUID.randomUUID(),
                        created: DateTime = nowWithoutMillis,
                        doi: String,
                        urn: String,
                       ) {

  def timestampString: String = created.toString(ISODateTimeFormat.dateTime())
}
object BagIndexInfo {
  private implicit val jsonFormats: Formats = new DefaultFormats {} +
    UUIDSerializer ++
    JodaTimeSerializers.all

  def apply(input: JsonInput): Try[BagIndexInfo] = {
    def acceptOnlyJObject(parsed: JValue): Try[Unit] = {
      if (parsed.isInstanceOf[JObject]) Success(())
      else Failure(new Exception(s"expected an object, got a ${ parsed.getClass }"))
    }

    for {
      parsed <- Try { JsonMethods.parse(input) }
      _ <- acceptOnlyJObject(parsed)
      result = Extraction.extract(parsed)
    } yield result
  }

  private def nowWithoutMillis: DateTime = {
    val now = DateTime.now(UTC)
    now.minusMillis(now.millisOfSecond().get())
  }
}
