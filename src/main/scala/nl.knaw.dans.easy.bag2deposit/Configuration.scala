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
import better.files.File.root
import nl.knaw.dans.easy.bag2deposit.collections.Collections.getCollectionsMap
import nl.knaw.dans.easy.bag2deposit.ddm.DdmTransformer
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.configuration.PropertiesConfiguration

import java.net.URI

case class Configuration(version: String,
                         dansDoiPrefixes: Seq[String],
                         dataverseIdAutority: String,
                         bagIndex: BagIndex,
                         ddmTransformer: DdmTransformer,
                        )

object Configuration extends DebugEnhancedLogging {

  def apply(home: File): Configuration = {
    val cfgPath = Seq(
      root / "etc" / "opt" / "dans.knaw.nl" / "easy-convert-bag-to-deposit",
      home / "cfg")
      .find(_.exists)
      .getOrElse { throw new IllegalStateException("No configuration directory found") }
    val properties = new PropertiesConfiguration() {
      setDelimiterParsingDisabled(true)
      load((cfgPath / "application.properties").toJava)
    }
    val version = (home / "bin" / "version").contentAsString.stripLineEnd
    val agent = properties.getString("http.agent", s"easy-convert-bag-to-deposit/$version")
    logger.info(s"setting http.agent to $agent")
    System.setProperty("http.agent", agent)

    new Configuration(
      version,
      dansDoiPrefixes = properties.getStringArray("dans-doi.prefixes"),
      dataverseIdAutority = properties.getString("dataverse.id-authority"),
      bagIndex = BagIndex(new URI(properties.getString("bag-index.url"))),
      ddmTransformer = DdmTransformer(cfgPath, getCollectionsMap(cfgPath, properties)),
    )
  }
}
