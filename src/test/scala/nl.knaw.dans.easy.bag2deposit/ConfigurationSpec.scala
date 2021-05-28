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
import nl.knaw.dans.easy.bag2deposit.Fixture.FileSystemSupport
import nl.knaw.dans.easy.bag2deposit.collections.Collection.getCollectionsMap
import nl.knaw.dans.easy.bag2deposit.collections.FedoraProvider
import nl.knaw.dans.easy.bag2deposit.ddm.DdmTransformer
import org.apache.commons.configuration.PropertiesConfiguration
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class ConfigurationSpec extends AnyFlatSpec with FileSystemSupport with Matchers {

  private def transformer(propsFile: File) = {
    val properties = new PropertiesConfiguration() {
      setDelimiterParsingDisabled(true)
      load(propsFile.toJava)
    }
    val cfgPath = propsFile.parent
    new DdmTransformer(cfgPath, getCollectionsMap(cfgPath, FedoraProvider(properties)))
  }

  "constructor" should "get past the first transformation when fedora is not configured" in {
    transformer(propsFile(fedoraUrl = "")).transform(
      <ddm><profile><audience>D37000</audience></profile></ddm>,
      "easy-dataset:123",
    ) shouldBe a[Success[_]]
  }

  it should "no longer fail on the first transformation when fedora is not available" in {
    val props = propsFile(fedoraUrl = "https://does.not.exist.dans.knaw.nl")
    // the lazy constructor argument throws an exception
    // breaking through the Try of the first call that needs it
    // this is not handled within the context of a for comprehension
    val triedNode = transformer(props).transform(
      <ddm><profile><audience>D37000</audience></profile></ddm>,
      "easy-dataset:123",
    )
    triedNode shouldBe Success(<ddm><profile><audience>D37000</audience></profile></ddm>)
  }
}
