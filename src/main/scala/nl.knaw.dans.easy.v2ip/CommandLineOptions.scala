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
package nl.knaw.dans.easy.v2ip

import java.nio.file.Path

import better.files.File
import nl.knaw.dans.easy.v2ip.IdType.IdType
import org.rogach.scallop.{ ScallopConf, ScallopOption, ValueConverter, singleArgConverter }

class CommandLineOptions(args: Array[String], configuration: Configuration) extends ScallopConf(args) {
  appendDefaultToDescription = true
  editBuilder(_.setHelpWidth(110))
  printedName = "easy-vault-export-ip"
  version(configuration.version)
  private val SUBCOMMAND_SEPARATOR = "---\n"
  val description: String = s"""Add deposit.properties to directories(s) with a bag"""
  val synopsis: String =
    s"""
       |  $printedName { -u <id> | -i <input-file> } -o <staged-IP-dir> -t [ URN | DOI ] [-l <log-file>]
       |""".stripMargin

  version(s"$printedName v${ configuration.version }")
  banner(
    s"""
       |  $description
       |
       |Usage:
       |
       |$synopsis
       |
       |Options:
       |""".stripMargin)

  implicit val fileConverter: ValueConverter[File] = singleArgConverter(File(_))
  implicit val idTypeConverter: ValueConverter[IdType] = singleArgConverter(IdType.withName)

  val sipDir: ScallopOption[File] = opt[Path]("sip", noshort = true,
    descr = "A directory containing nothing but a bag").map(File(_))
  val sipDirs: ScallopOption[File] = opt[Path]("sips", noshort = true,
    descr = "A directory with directories containing nothing but a bag").map(File(_))
  val idType: ScallopOption[IdType] = opt[IdType]("dataverse-identifier-type", short = 't', required = true,
    descr = "the field to be used as Dataverse identifier, either doi or urn:nbn")
  val outputDir: ScallopOption[File] = opt(name = "output-dir", short = 'o', required = false,
    descr = "Empty directory that will receive completed SIPs with atomic moves. It will be created if it does not exist.")

  requireOne(sipDir, sipDirs)
  validate(outputDir)(dir => {
    if (dir.exists) {
      if (!dir.isDirectory) Left(s"outputDir $dir does not reference a directory")
      else if (dir.nonEmpty) Left(s"outputDir $dir exists but is not an empty directory")
           else if (!dir.isWriteable) Left(s"outputDir $dir exists and is empty but is not writeable by the current user")
                else Right(())
    }
    else {
      dir.createDirectories()
      Right(())
    }
  })

  footer("")
}
