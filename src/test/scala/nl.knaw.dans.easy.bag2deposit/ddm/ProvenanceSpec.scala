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
import nl.knaw.dans.easy.bag2deposit.Fixture.{FileSystemSupport, FixedCurrentDateTimeSupport, SchemaSupport, XmlSupport}
import nl.knaw.dans.easy.bag2deposit.{AmdTransformer, loadXml}
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.Charset
import scala.xml.{Utility, XML}

class ProvenanceSpec extends AnyFlatSpec with FileSystemSupport with XmlSupport with Matchers with FixedCurrentDateTimeSupport with DebugEnhancedLogging with SchemaSupport {
  // use actual location (and replace in validated XML) when upgraded scheme is not yet published
  private val actualLocation = "https://raw.githubusercontent.com/DANS-KNAW/easy-schema/DD-811-encoding/lib/src/main/resources"
  private val defaultLocation = "https://easy.dans.knaw.nl/schemas"
  override val schema: String = defaultLocation + "/bag/metadata/prov/provenance.xsd"

  "Provenance" should "show encoding changes" in {
    val ddmOut = XML.loadFile("src/test/resources/encoding/ddm-out.xml")
    val (ddmIn, oldChars, newChars) = loadXml(File("src/test/resources/encoding/ddm-in.xml"))
      .getOrElse(throw new IllegalArgumentException("could not load test data"))
    val provenance = new Provenance("EasyConvertBagToDepositApp", "1.0.5")
      .collectChangesInXmls(List(
        Provenance.fixedDdmEncoding(oldChars, newChars),
        Provenance.compare((ddmIn \ "profile").head, (ddmOut \ "profile").head, "http://easy.dans.knaw.nl/schemas/md/ddm/"),
        Provenance.compare((ddmIn \ "dcmiMetadata").head, (ddmOut \ "dcmiMetadata").head, "http://easy.dans.knaw.nl/schemas/md/ddm/"),
      ))
    // when loading XML, "<" in CDATA is interpreted into a plain string with "&lt;", so we have to re-parse the generated xml to compare
    normalized(XML.loadString(Utility.serialize(provenance).toString())) shouldBe
      normalized(XML.loadFile("src/test/resources/encoding/provenance.xml"))

    assume(schemaIsAvailable)
    val xmlString = File("src/test/resources/encoding/provenance.xml")
      .contentAsString(Charset.forName("UTF-8"))
//      .replaceAll(" " + defaultLocation, " " + actualLocation)
    validate(XML.loadString(xmlString))
  }
  "compare" should "show ddm diff" in {
    val ddmIn = {
      <ddm>
        <ddm:profile>
          <dc:title>Rapport 123</dc:title>
        </ddm:profile>
        <ddm:dcmiMetadata>
          <dc:title>blabla</dc:title>
          <dc:title>Rapport 456</dc:title>
          <dc:title>Transect-rapport 2859: Een Archeologisch Bureauonderzoek. Ellecom, glasvezeltracé Eikenstraat, Gemeente Rheden (GD).</dc:title>
          <dc:title>rabarbera</dc:title>
          <dc:title>Archeologische Berichten Nijmegen – Briefrapport 21</dc:title>
          <dcterms:temporal xsi:type="abr:ABRperiode">VMEA</dcterms:temporal>
          <dc:subject xsi:type="abr:ABRcomplex">EGVW</dc:subject>
          <dcterms:subject xsi:type="abr:ABRcomplex">ELA</dcterms:subject>
        </ddm:dcmiMetadata>
      </ddm>
    }

    val ddmOut = {
      <ddm>
        <ddm:profile>
          <dc:title>Rapport 123</dc:title>
        </ddm:profile>
        <ddm:dcmiMetadata>
          <dc:title>blabla</dc:title>
          <ddm:reportNumber
          schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e"
          valueURI="https://data.cultureelerfgoed.nl/term/id/abr/fcff6035-9e90-450f-8b39-cf33447e6e9f"
          subjectScheme="ABR Rapporten"
          reportNo="456">Rapport 456</ddm:reportNumber>
          <ddm:reportNumber
          schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e"
          valueURI="https://data.cultureelerfgoed.nl/term/id/abr/90f3092a-818e-4db2-8467-35b64262c5b3"
          subjectScheme="ABR Rapporten"
          reportNo="2859">Transect-rapport 2859</ddm:reportNumber>
          <dc:title>Transect-rapport 2859: Een Archeologisch Bureauonderzoek. Ellecom, glasvezeltracé Eikenstraat, Gemeente Rheden (GD).</dc:title>
          <dc:title>rabarbera</dc:title>
          <dc:title>Archeologische Berichten Nijmegen – Briefrapport 21</dc:title>
          <ddm:temporal xml:lang="nl"
                        valueURI="https://data.cultureelerfgoed.nl/term/id/abr/330e7fe0-a1f7-43de-b448-d477898f6648"
                        subjectScheme="ABR Perioden"
                        schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/b6df7840-67bf-48bd-aa56-7ee39435d2ed">Vroege Middeleeuwen A</ddm:temporal>
          <ddm:subject xml:lang="nl"
                       valueURI="https://data.cultureelerfgoed.nl/term/id/abr/6ae3ab19-49ca-44a7-8b65-3a3395014bb9"
                       subjectScheme="ABR Complextypen"
                       schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/b6df7840-67bf-48bd-aa56-7ee39435d2ed">veenwinning (inclusief zouthoudend veen t.b.v. zoutproductie)</ddm:subject>
          <ddm:subject xml:lang="nl"
                       valueURI="https://data.cultureelerfgoed.nl/term/id/abr/f182d72c-2d22-47ae-b799-26dea01e770c"
                       subjectScheme="ABR Complextypen"
                       schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/b6df7840-67bf-48bd-aa56-7ee39435d2ed">akker / tuin</ddm:subject>
          <ddm:reportNumber
          schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e"
          valueURI="https://data.cultureelerfgoed.nl/term/id/abr/fcff6035-9e90-450f-8b39-cf33447e6e9f"
          subjectScheme="ABR Rapporten"
          reportNo="123">Rapport 123</ddm:reportNumber>
          <dct:rightsHolder>Unknown</dct:rightsHolder>
        </ddm:dcmiMetadata>
      </ddm>
    }

    Provenance.compare((ddmIn \ "dcmiMetadata").head, (ddmOut \ "dcmiMetadata").head, "http://easy.dans.knaw.nl/schemas/md/ddm/")
      .map(normalized) shouldBe Some(normalized(
      <prov:file scheme="http://easy.dans.knaw.nl/schemas/md/ddm/">
        <prov:old>
          <dc:title>Rapport 456</dc:title> <dcterms:temporal xsi:type="abr:ABRperiode">VMEA</dcterms:temporal> <dc:subject xsi:type="abr:ABRcomplex">EGVW</dc:subject> <dcterms:subject xsi:type="abr:ABRcomplex">ELA</dcterms:subject>
        </prov:old>
        <prov:new>
          <ddm:reportNumber schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e" valueURI="https://data.cultureelerfgoed.nl/term/id/abr/fcff6035-9e90-450f-8b39-cf33447e6e9f" subjectScheme="ABR Rapporten" reportNo="456">Rapport 456</ddm:reportNumber> <ddm:reportNumber schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e" valueURI="https://data.cultureelerfgoed.nl/term/id/abr/90f3092a-818e-4db2-8467-35b64262c5b3" subjectScheme="ABR Rapporten" reportNo="2859">Transect-rapport 2859</ddm:reportNumber> <ddm:temporal xml:lang="nl" valueURI="https://data.cultureelerfgoed.nl/term/id/abr/330e7fe0-a1f7-43de-b448-d477898f6648" subjectScheme="ABR Perioden" schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/b6df7840-67bf-48bd-aa56-7ee39435d2ed">Vroege Middeleeuwen A</ddm:temporal> <ddm:subject xml:lang="nl" valueURI="https://data.cultureelerfgoed.nl/term/id/abr/6ae3ab19-49ca-44a7-8b65-3a3395014bb9" subjectScheme="ABR Complextypen" schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/b6df7840-67bf-48bd-aa56-7ee39435d2ed">veenwinning (inclusief zouthoudend veen t.b.v. zoutproductie)</ddm:subject>
          <ddm:subject xml:lang="nl" valueURI="https://data.cultureelerfgoed.nl/term/id/abr/f182d72c-2d22-47ae-b799-26dea01e770c" subjectScheme="ABR Complextypen" schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/b6df7840-67bf-48bd-aa56-7ee39435d2ed">akker / tuin</ddm:subject>
          <ddm:reportNumber schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e" valueURI="https://data.cultureelerfgoed.nl/term/id/abr/fcff6035-9e90-450f-8b39-cf33447e6e9f" subjectScheme="ABR Rapporten" reportNo="123">Rapport 123</ddm:reportNumber>
          <dct:rightsHolder>Unknown</dct:rightsHolder>
        </prov:new>
      </prov:file>
    ))
  }
  it should "show dropped zero point" in {
    val ddmIn = {
      <ddm>
        <ddm:profile>
          <dc:title>blabla</dc:title>
        </ddm:profile>
        <ddm:dcmiMetadata>
          <dcx-gml:spatial srsName="http://www.opengis.net/def/crs/EPSG/0/28992">
            <gml:Point>
              <gml:pos>0 0</gml:pos>
            </gml:Point>
          </dcx-gml:spatial>
          <dcx-gml:spatial srsName="http://www.opengis.net/def/crs/EPSG/0/28992">
            <Point xmlns="http://www.opengis.net/gml">
              <description>Entrance of DANS Building</description>
              <name>Data Archiving and Networked Services (DANS)</name>
              <pos>0 0</pos>
            </Point>
          </dcx-gml:spatial>
        </ddm:dcmiMetadata>
      </ddm>
    }

    val ddmOut = {
      <ddm>
        <ddm:profile>
          <dc:title>blabla</dc:title>
        </ddm:profile>
        <ddm:dcmiMetadata>
          <dcx-gml:spatial srsName="http://www.opengis.net/def/crs/EPSG/0/28992">
            <Point xmlns="http://www.opengis.net/gml">
              <description>Entrance of DANS Building</description>
              <name>Data Archiving and Networked Services (DANS)</name>
              <pos>0 0</pos>
            </Point>
          </dcx-gml:spatial>
        </ddm:dcmiMetadata>
      </ddm>
    }

    Provenance.compare((ddmIn \ "dcmiMetadata").head, (ddmOut \ "dcmiMetadata").head, "http://easy.dans.knaw.nl/schemas/md/ddm/")
      .map(normalized) shouldBe Some(normalized(
          <prov:file scheme="http://easy.dans.knaw.nl/schemas/md/ddm/">
            <prov:old>
              <dcx-gml:spatial srsName="http://www.opengis.net/def/crs/EPSG/0/28992">
                <gml:Point>
                  <gml:pos>0 0</gml:pos>
                </gml:Point>
              </dcx-gml:spatial>
            </prov:old>
            <prov:new>
            </prov:new>
          </prov:file>
    ))
  }
  it should "show funder diff" in {
    // compare the DDM files manually for finer details than in the provenance
    val ddmIn = XML.loadFile("src/test/resources/funder/ddm-in.xml")
    val ddmOut = XML.loadFile("src/test/resources/funder/ddm-out.xml")
    val expected = XML.loadFile("src/test/resources/funder/provenance.xml")

    Provenance.compare(
      (ddmIn \ "dcmiMetadata").head,
      (ddmOut \ "dcmiMetadata").head,
      "http://easy.dans.knaw.nl/schemas/md/ddm/"
    ).map(normalized).map(dropAttrs) shouldBe Some(normalized(expected)).map(dropAttrs)
  }

  private def dropAttrs(s: String) = {
    s.replaceAll("<prov:migration[^>]*", "<prov:migration") // get rid of random order attributes
      .replaceAll("(?m)<prov:provenance[^>]*>", "") // get rid of random order in xsi:schemaLocation=
  }

  it should "show amd diff" in {
    (testDir / "amd.xml").writeText(
      """<?xml version="1.0" encoding="UTF-8"?>""" +
        Utility.serialize(
          <damd:administrative-md xmlns:damd="http://easy.dans.knaw.nl/easy/dataset-administrative-metadata/" version="0.1">
            <datasetState>PUBLISHED</datasetState>
            <previousState>DRAFT</previousState>
            <lastStateChange>2020-02-02T20:02:00.000+01:00</lastStateChange>
            <depositorId>user001</depositorId>
            <stateChangeDates>
              <damd:stateChangeDate>
                <fromState>SUBMITTED</fromState>
                <toState>PUBLISHED</toState>
                <changeDate>2017-05-02T13:01:26.752+02:00</changeDate>
              </damd:stateChangeDate>
              <damd:stateChangeDate>
                <fromState>PUBLISHED</fromState>
                <toState>MAINTENANCE</toState>
                <changeDate>2017-10-13T09:31:55.215+02:00</changeDate>
              </damd:stateChangeDate>
              <damd:stateChangeDate>
                <fromState>MAINTENANCE</fromState>
                <toState>PUBLISHED</toState>
                <changeDate>2017-10-13T09:35:01.605+02:00</changeDate>
              </damd:stateChangeDate>
            </stateChangeDates>
          </damd:administrative-md>
        ).toString()
    )

    val transformer = new AmdTransformer(cfgDir = File("src/main/assembly/dist/cfg"))
    val amdIn = XML.loadFile((testDir / "amd.xml").toJava)
    val created = <ddm:created>2016-31-12</ddm:created>
    val amdOut = transformer.transform(amdIn, created).getOrElse(fail("could not transform"))
    amdOut.text shouldNot include("2017-05-02T13:01:26.752+02:00")
    amdOut.text should include("2016-31-12")
    Provenance.compare(amdIn, amdOut, "http://easy.dans.knaw.nl/easy/dataset-administrative-metadata/")
      .map(normalized) shouldBe Some(normalized(
          <prov:file scheme="http://easy.dans.knaw.nl/easy/dataset-administrative-metadata/">
            <prov:old>
              <depositorId>user001</depositorId>
              <damd:stateChangeDate>
                <fromState>SUBMITTED</fromState>
                <toState>PUBLISHED</toState>
                <changeDate>2017-05-02T13:01:26.752+02:00</changeDate>
              </damd:stateChangeDate>
            </prov:old>
            <prov:new>
              <depositorId>USer</depositorId>
              <damd:stateChangeDate>
                <fromState>SUBMITTED</fromState>
                <toState>PUBLISHED</toState>
                <changeDate>2016-31-12</changeDate>
              </damd:stateChangeDate>
            </prov:new>
          </prov:file>
    ))
  }
}
