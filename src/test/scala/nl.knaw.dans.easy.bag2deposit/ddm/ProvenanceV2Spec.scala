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
package nl.knaw.dans.easy.bag2deposit.ddm

import nl.knaw.dans.easy.bag2deposit.DdmVersion.V2
import nl.knaw.dans.easy.bag2deposit.Fixture._
import nl.knaw.dans.easy.bag2deposit.XmlExtensions
import nl.knaw.dans.easy.bag2deposit.ddm.DdmTransformer.ddmV2namespace
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class ProvenanceV2Spec extends AnyFlatSpec with FileSystemSupport with XmlSupport with Matchers with FixedCurrentDateTimeSupport with DebugEnhancedLogging with SchemaSupport with AppConfigSupport {
  // use the raw github location while upgraded schema is not yet published, your own fork if not yet merged.
  // TODO download from maven into target folder
  override val schema: String = "https://raw.githubusercontent.com/DANS-KNAW/dans-schema/master/src/main/resources/bag/metadata/prov/v2/provenance.xsd"

  // FixedCurrentDateTimeSupport is not effective for a val
  private def provenanceBuilder = Provenance("EasyConvertBagToDepositApp", "1.0.5")

  "Provenance" should "change namespace" in {
    val ddmIn = {
      <ddm:DDM xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
               xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
               xsi:schemaLocation=" http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd"
               xmlns:gml="http://www.opengis.net/gml"
      >
        <ddm:profile>
          <dc:title>blabla</dc:title>
        </ddm:profile>
      </ddm:DDM>
    }

    val expectedDdm = <ddm:DDM xmlns:ddm={ ddmV2namespace }
             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
             xmlns:gml="http://www.opengis.net/gml"
    >
      <ddm:profile>
        <dc:title>blabla</dc:title>
        <ddm:personalData present="Yes"/>
      </ddm:profile>
    </ddm:DDM>

    val expectedProv = {
      <prov:provenance xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                       xmlns:prov="http://schemas.dans.knaw.nl/bag/metadata/prov/v2/"
      >
        <prov:migration app="EasyConvertBagToDepositApp" version="1.0.5" date="2020-02-02">
        <prov:file scheme={ ddmV2namespace } xmlns=""
                   xmlns:ddm={ ddmV2namespace }
                   xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                   xmlns:gml="http://www.opengis.net/gml"
        >
        <prov:old/>
        <prov:new><ddm:personalData present="Yes"/></prov:new>
          </prov:file>
        </prov:migration>
      </prov:provenance>
    }

    // a few steps of EasyConvertBagToDepositApp.addProps
    val ddmOut = testConfig("archaeology").copy(ddmVersion = V2).ddmTransformer
      .transform(ddmIn, "easy-dataset:123", containsPrivacySensitiveData = "true")
      .getOrElse(fail("no DDM returned"))
    val actualProv = provenanceBuilder.collectChangesInXmls(List(Provenance.compareDDM(ddmIn, ddmOut)))

    // check what was stripped by normalize to avoid random order
    ddmOut.serialize should include(ddmV2namespace)
    actualProv.serialize should include(ddmV2namespace)

    normalized(ddmOut) shouldBe normalized(expectedDdm)
    normalized(actualProv).replaceAll(" +scheme"," scheme") shouldBe normalized(expectedProv).replaceAll(" +scheme"," scheme")
    assume(schemaIsAvailable)
    validate(expectedProv) shouldBe a[Success[_]]
  }
}
