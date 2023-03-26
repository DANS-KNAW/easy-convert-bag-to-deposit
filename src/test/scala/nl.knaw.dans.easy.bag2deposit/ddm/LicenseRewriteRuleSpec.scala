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

import better.files.File
import nl.knaw.dans.easy.bag2deposit.ddm
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LicenseRewriteRuleSpec extends AnyFlatSpec with Matchers {
  private val rule: LicenseRewriteRule = ddm.LicenseRewriteRule(File("src/main/assembly/dist/cfg"))

  "transform <license>" should "not change" in {
    val input = Seq(
      <dct:license xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dct="http://purl.org/dc/terms/" xsi:type="dct:URI">
        http://creativecommons.org/licenses/by-nc-sa/4.0</dct:license>,
    )
    input.flatMap(rule.transform).text shouldBe "http://creativecommons.org/licenses/by-nc-sa/4.0"
  }

 it should "change https into http" in {
   val input = Seq(
       <dct:license xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dct="http://purl.org/dc/terms/" xsi:type="dct:URI">https://creativecommons.org/publicdomain/zero/1.0</dct:license>,
   )
   input.flatMap(rule.transform).text shouldBe "http://creativecommons.org/publicdomain/zero/1.0"
 }

  it should "keep the same namespace, dcterms" in {
    val input = Seq(
        <dcterms:license xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dcterms="http://purl.org/dc/terms/" xsi:type="dcterms:URI">https://creativecommons.org/publicdomain/zero/1.0</dcterms:license>,
    )
    input.flatMap(rule.transform).head.prefix shouldBe "dcterms"
    input.flatMap(rule.transform).text shouldBe "http://creativecommons.org/publicdomain/zero/1.0"
  }

  it should "keep the same namespace, dct" in {
    val input = Seq(
        <dct:license xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dct="http://purl.org/dc/terms/" xsi:type="dct:URI">https://creativecommons.org/publicdomain/zero/1.0</dct:license>,
    )
    input.flatMap(rule.transform).head.prefix shouldBe "dct"
    input.flatMap(rule.transform).text shouldBe "http://creativecommons.org/publicdomain/zero/1.0"
  }

  it should "be a variant" in {
    val input = Seq(
     <dct:license xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dct="http://purl.org/dc/terms/" xsi:type="dct:URI">http://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html</dct:license>,
    )
    input.flatMap(rule.transform).text shouldBe "http://www.gnu.org/licenses/old-licenses/gpl-2.0"
  }
}
