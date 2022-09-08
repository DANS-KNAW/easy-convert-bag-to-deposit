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

import better.files.File
import nl.knaw.dans.easy.bag2deposit.TargetDataStation.TargetDataStation
import nl.knaw.dans.easy.bag2deposit.collections.Collection.getCollectionsMap
import nl.knaw.dans.easy.bag2deposit.collections.FedoraProvider
import nl.knaw.dans.easy.bag2deposit.ddm.DdmTransformer

case class Configuration(version: String,
                         dansDoiPrefixes: Seq[String],
                         dataverseIdAuthority: String,
                         bagIndex: BagIndex,
                         bagSequence: Boolean,
                         maybeFedoraProvider: Option[FedoraProvider],
                         maybePreStagedProvider: Option[PreStagedProvider],
                         cfgPath: File,
                         targetDataStation: TargetDataStation,
                        )
{
  private val targetCfgPath = cfgPath / targetDataStation.toString
  val agreementsPath: File = cfgPath / "agreements"
  val ddmTransformer = new DdmTransformer(cfgPath, targetDataStation, getCollectionsMap(targetCfgPath))
  val amdTransformer = new AmdTransformer(targetCfgPath / "account-substitutes.csv")
}
