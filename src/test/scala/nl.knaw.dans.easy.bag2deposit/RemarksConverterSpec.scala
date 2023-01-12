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
import nl.knaw.dans.easy.bag2deposit.Fixture.{ FileSystemSupport, SchemaSupport, XmlSupport }
import nl.knaw.dans.easy.bag2deposit.ddm.DdmTransformer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

import scala.util.Success

class RemarksConverterSpec extends AnyFlatSpec with XmlSupport with Matchers with FileSystemSupport with SchemaSupport { // use actual location (and replace in validated XML) when upgraded schema is not yet published
  private val actualLocation = "https://raw.githubusercontent.com/DANS-KNAW/easy-schema/DD-811-encoding/lib/src/main/resources"
  private val defaultLocation = "https://easy.dans.knaw.nl/schemas"
  override val schema: String = defaultLocation + "/md/ddm/ddm.xsd"

  private val datasetId = "easy-dataset:162288"
  private val doi = "10.17026/dans-x6j-xz6u"
  private val cfgDir = testDir / "cfg"

  private val ddmIn = <ddm:DDM xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xmlns:dc="http://purl.org/dc/elements/1.1/"
    xmlns:dct="http://purl.org/dc/terms/"
    xmlns:id-type="http://easy.dans.knaw.nl/schemas/vocab/identifier-type/"
    xsi:schemaLocation="
    http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd
        http://www.loc.gov/mods/v3 http://www.loc.gov/standards/mods/v3/mods-3-7.xsd
        http://easy.dans.knaw.nl/schemas/vocab/identifier-type/ https://easy.dans.knaw.nl/schemas/vocab/identifier-type.xsd
    ">
    <ddm:profile>
      <dc:title>Lorum ipsum</dc:title>
      <dc:description xml:lang="la">dolor sit amet</dc:description>
      <dc:creator>me</dc:creator>
      <ddm:created>2012-12</ddm:created>
      <ddm:available>2013-05</ddm:available>
      <ddm:audience>D20000</ddm:audience>
      <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
    </ddm:profile>
    <ddm:dcmiMetadata/>
  </ddm:DDM>

  private def configureDdmTransformer() = {
    cfgDir.delete(swallowIOExceptions = true)
    cfgDir.createDirectory()
    (cfgDir / "SSH").createDirectory()
    (cfgDir / "dataset-doi.csv").writeText(
      s"""datasetid,doi
         |$datasetId,$doi
         |""".stripMargin)
    (cfgDir / "urn-nbn-doi.csv").writeText(
      s"""urn-nbn,doi
         |urn:nbn:nl:ui:13-01gf-2z,$doi
         |""".stripMargin)
    File("src/main/assembly/dist/cfg/languages.csv")
      .copyTo(cfgDir / "languages.csv")
    new DdmTransformer(cfgDir, "SSH", Map.empty)
  }

  private def convert(ddmTransformer: DdmTransformer,
                      remarksConverter: RemarksConverter = new RemarksConverter(cfgDir / "SSH"),
                      emdContent: String = "<emd><eas:remark>Just some remark for testing purposes</eas:remark></emd>",
                     ) = {
    val emdFile = testDir / "emd.xml"
    emdFile.writeText(emdContent)
    val additional = remarksConverter.additionalDcmi(emdFile, datasetId, fromVault = false).get
    ddmTransformer.transform(ddmIn, datasetId, additional).get
  }

  private def writeRemarksMappingCSV(category: String) = {
    (cfgDir / "SSH" / "remarks-mapping.csv").writeText(
      s"""easy-dataset:id,category
         |$datasetId,$category
         |""".stripMargin
    )
  }

  "additionalDcmi" should "apply the default description for a missing mapping" in {
    val ddmTransformer = configureDdmTransformer()
    val ddmOut = convert(ddmTransformer)
    ddmOut.serialize should include("""<dct:description>Just some remark for testing purposes</""")
    assume(schemaIsAvailable)
    validate(ddmOut) shouldBe a[Success[_]]
  }
  it should "ignore empty remarks" in {
    val ddmTransformer = configureDdmTransformer()
    writeRemarksMappingCSV("funder")
    val ddmOut = convert(ddmTransformer, emdContent = "<emd><eas:remark></eas:remark></emd>")
    ddmOut.serialize shouldNot include("""<ddm:description""")
  }
  it should "not add the remark to the DDM" in {
    val ddmTransformer = configureDdmTransformer()
    writeRemarksMappingCSV("ignore")
    convert(ddmTransformer).serialize shouldNot
      include("""<dct:description>Just some remark for testing purposes</""")
  }
  it should "produce valid mappings" in {
    assume(schemaIsAvailable)
    val ddmTransformer = configureDdmTransformer()
    val otherDescription = """<ddm:description descriptionType="Other">"""
    val table = Table(("category", "expected"),
      ("access", "<dct:accessRights>"),
      ("citation", "<dct:bibliographicCitation>"),
      ("contact", otherDescription),
      ("contributor", otherDescription),
      ("copyright", "<dct:rightsHolder>"),
      ("description", "<dct:description>"),
      ("files", """<ddm:description descriptionType="TechnicalInfo">"""),
      ("negeren", "<dct:description>"),// logs dataset-id for this invalid category
      ("funder", otherDescription),
      ("provenance", "<dct:provenance>"),
      ("relation", "<dc:relation>"),
      ("collectiondate", "<dc:date>"),
    )
    forAll(table) { (category: String, expected: String) =>
      // once a row fails, the rest is not executed
      writeRemarksMappingCSV(category)
      val ddmOut = convert(ddmTransformer)
      ddmOut.serialize should include(expected + "Just some remark for testing purposes</")
      validate(ddmOut) shouldBe a[Success[_]]
    }
  }
}
