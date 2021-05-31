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
package nl.knaw.dans.easy.bag2deposit.Fixture

import org.joda.time.{ DateTime, DateTimeUtils }
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec

trait FixedCurrentDateTimeSupport extends BeforeAndAfterEach {
  this: AnyFlatSpec =>

  override def beforeEach(): Unit = {
    super.beforeEach()
    DateTimeUtils.setCurrentMillisFixed(new DateTime("2020-02-02T20:20:02.000Z").getMillis)
  }

  override def afterEach(): Unit = {
    super.afterEach()
    DateTimeUtils.setCurrentMillisSystem()
  }
}
