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
package nl.knaw.dans.easy.bag2deposit

import nl.knaw.dans.easy.bag2deposit.Fixture.{ DdmSupport, FileSystemSupport, SchemaSupport }
import nl.knaw.dans.easy.bag2deposit.collections.Collection
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CollectionSpec extends AnyFlatSpec with DdmSupport with SchemaSupport with Matchers with FileSystemSupport with MockFactory {
  override val schema = "https://raw.githubusercontent.com/DANS-KNAW/easy-schema/eade34a3c05669d05ec8cdbeb91a085d83c6c030/lib/src/main/resources/md/2021/02/ddm.xsd"

  "getCollectionsMap" should "not stumble over <br> and combine multiple datasets into a single collection" in {
    val originalCsv =
      """naam,EASY-dataset-id,type,opmerkingen,members
        |Diachron bv,"easy-dataset:33834,easy-dataset:33976",organisation,,"easy-dataset:64188"
        |""".stripMargin
    val cfgDir = propsFile("").parent / TargetDataStation.archaeology.toString
    val csvFile = cfgDir / "ThemathischeCollecties.csv"
    csvFile.writeText(originalCsv)

    // just sampling one of the expected tuples, the keys of all tuples are written to the updated CSV
    val sampleElem =
        <ddm:inCollection
          schemeURI="https://vocabularies.dans.knaw.nl/collections"
          valueURI="https://vocabularies.dans.knaw.nl/collections/archaeology/Diachronbv"
          subjectScheme="DANS Collection"
        >Diachron bv</ddm:inCollection>

    Collection.getCollectionsMap(cfgDir).get("easy-dataset:64188").head.head shouldBe sampleElem
  }
}
