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

import nl.knaw.dans.easy.bag2deposit.{ BagIndex, PreStagedProvider }
import org.scalamock.scalatest.MockFactory
import scalaj.http.HttpResponse

trait PreStagedSupport extends MockFactory {

  // mock requires a constructor without parameters
  class MockPreStagedProvider() extends PreStagedProvider(null)

  // mock the HTTP-request execution to test the rest of the class and/or application
  def delegatingPreStagedProvider(delegate: PreStagedProvider): PreStagedProvider = new PreStagedProvider(null) {
    override def execute(q: String): HttpResponse[String] = delegate.execute(q)
  }
}
