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
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.language.reflectiveCalls

object Command extends App with DebugEnhancedLogging {
  type FeedBackMessage = String

  val configuration = Configuration(File(System.getProperty("app.home")))
  val commandLine: CommandLineOptions = new CommandLineOptions(args, configuration) {
    verify()
  }
  private val bagParentDirs = commandLine.bagParentDir.map(Iterator(_))
    .getOrElse(commandLine.bagGrandParentDir.map(_.children)
      .getOrElse(Iterator.empty))

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
}
