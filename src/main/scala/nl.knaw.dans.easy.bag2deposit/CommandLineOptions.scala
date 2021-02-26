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

import java.nio.file.Path

import better.files.File
import nl.knaw.dans.easy.bag2deposit.BagSource.BagSource
import nl.knaw.dans.easy.bag2deposit.IdType.IdType
import org.rogach.scallop.{ ScallopConf, ScallopOption, ValueConverter, singleArgConverter }

class CommandLineOptions(args: Array[String], configuration: Configuration) extends ScallopConf(args) {
  appendDefaultToDescription = true
  editBuilder(_.setHelpWidth(110))
  printedName = "easy-convert-bag-to-deposit"
  version(configuration.version)
  val description: String = s"""Add deposit.properties to directories(s) with a bag"""
  val synopsis: String =
    s"""
       |  $printedName { --dir | --uuid } <directory> -t { URN | DOI } -s { FEDORA | VAULT } [ -o <staged-IP-dir> ]
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
  implicit val bagSourceConverter: ValueConverter[BagSource] = singleArgConverter(BagSource.withName)

  val bagGrandParentDir: ScallopOption[File] = opt[Path]("dir", short = 'd',
    descr = "directory with the deposits. These deposit-dirs each MUST have the uuid of the bag as directory name, and have one bag-dir each").map(File(_))
  val bagParentDir: ScallopOption[File] = opt[Path]("uuid", short = 'u',
    descr = "directory with a bag. This directory each MUST be a uuid.").map(File(_))
  val idType: ScallopOption[IdType] = opt[IdType]("dataverse-identifier-type", short = 't', required = true,
    descr = "the field to be used as Dataverse identifier, either doi or urn:nbn")
  val bagSource: ScallopOption[BagSource] = opt[BagSource]("source", short = 's', required = true,
    descr = "The source of the bags")
  val outputDir: ScallopOption[File] = opt(name = "output-dir", short = 'o', required = false,
    descr = "Optional. Directory that will receive completed deposits with atomic moves.")

  requireOne(bagParentDir, bagGrandParentDir)
  validate(outputDir) { dir =>
    if (!dir.isDirectory) Left(s"outputDir $dir does not reference a directory")
    else Right(())
  }
  validate(bagSource) {
    case BagSource.FEDORA if idType() != IdType.DOI => Left(s"source FEDORA requires dataverse-identifier-type=DOI")
    case _ => Right(())
  }

  footer("")
}
