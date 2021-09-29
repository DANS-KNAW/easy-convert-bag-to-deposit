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
package nl.knaw.dans.easy.bag2deposit.Fixture

import better.files.File
import nl.knaw.dans.easy.bag2deposit.collections.FedoraProvider
import nl.knaw.dans.easy.bag2deposit.ddm.DdmTransformer
import nl.knaw.dans.easy.bag2deposit.{BagIndex, Configuration, UserTransformer}

trait AppConfigSupport extends BagIndexSupport with FedoraProviderSupport {
  def testConfig(bagIndex: BagIndex, fedoraProvider: Option[FedoraProvider]): Configuration = {
    val cfgFile = File("src/main/assembly/dist/cfg")
    new Configuration(
      version = "testVersion",
      dansDoiPrefixes = Seq("10.17026", "10.5072"),
      dataverseIdAuthority = "10.80270",
      bagIndex = bagIndex,
      ddmTransformer = new DdmTransformer(cfgFile),
      userTransformer = new UserTransformer(cfgFile),
      fedoraProvider = fedoraProvider,
    )
  }
}
