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
import better.files.File.root
import nl.knaw.dans.easy.bag2deposit.collections.FedoraProvider
import nl.knaw.dans.lib.error.TryExtensions
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.configuration.PropertiesConfiguration

import java.net.URI
import scala.language.reflectiveCalls

object Command extends App with DebugEnhancedLogging {
  type FeedBackMessage = String
  private val home = File(System.getProperty("app.home"))
  private val cfgPath = Seq(
    root / "etc" / "opt" / "dans.knaw.nl" / "easy-convert-bag-to-deposit",
    home / "cfg")
    .find(_.exists)
    .getOrElse { throw new IllegalStateException("No configuration directory found") }
  private val properties = {
    new PropertiesConfiguration() {
      setDelimiterParsingDisabled(true)
      load((cfgPath / "application.properties").toJava)
    }
  }
  private val version = (home / "bin" / "version").contentAsString.stripLineEnd
  private val agent = properties.getString("http.agent", s"easy-convert-bag-to-deposit/$version")
  logger.info(s"setting http.agent to $agent")
  System.setProperty("http.agent", agent)

  private val commandLine: CommandLineOptions = new CommandLineOptions(args, version) {
    verify()
  }
  {
    val dir = cfgPath / commandLine.target()
    if (!dir.isDirectory)
      throw new IllegalArgumentException(s"$dir should be an existing directory: ${ commandLine.target.name }")
  }
  private val bagParentDirs = commandLine.bagParentDir.map(Iterator(_))
    .getOrElse(commandLine.bagGrandParentDir.map(_.children)
      .getOrElse(Iterator.empty))

  private val configuration = Configuration(
    version,
    dansDoiPrefixes = properties.getStringArray("dans-doi.prefixes"),
    dataverseIdAuthority = properties.getString("dataverse.id-authority"),
    bagIndex = BagIndex(new URI(properties.getString("bag-index.url"))),
    bagSequence = commandLine.bagSequence(),
    maybeFedoraProvider = FedoraProvider(properties),
    cfgPath,
    commandLine.target(),
  )
  private val propertiesFactory = DepositPropertiesFactory(
    configuration,
    commandLine.idType(),
    commandLine.bagSource()
  )
  new EasyConvertBagToDepositApp(configuration)
    .addPropsToBags(
      bagParentDirs,
      commandLine.outputDir.toOption,
      propertiesFactory
    ).map(msg => println(s"$msg, for details see logging"))
    .doIfFailure { case e =>
      println(s"$e; for details see logging")
    }
}
