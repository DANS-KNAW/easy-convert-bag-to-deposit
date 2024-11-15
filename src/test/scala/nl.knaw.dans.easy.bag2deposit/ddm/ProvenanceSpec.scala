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
import nl.knaw.dans.easy.bag2deposit.Fixture._
import nl.knaw.dans.easy.bag2deposit.{ AmdTransformer, loadXml }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{ Failure, Success }
import scala.xml.{ Utility, XML }

class ProvenanceSpec extends AnyFlatSpec with FileSystemSupport with XmlSupport with Matchers with FixedCurrentDateTimeSupport with DebugEnhancedLogging with SchemaSupport with AppConfigSupport {
  // use the raw github location while upgraded schema is not yet published, your own fork if not yet merged.
  private val schemaRoot = "https://easy.dans.knaw.nl/schemas"
  override val schema: String = schemaRoot + "bag/metadata/prov/provenance.xsd"
  private val schemaLocation = s"http://easy.dans.knaw.nl/schemas/bag/metadata/prov/ $schema"

  // FixedCurrentDateTimeSupport is not effective for a val
  private def provenanceBuilder = Provenance("EasyConvertBagToDepositApp", "1.0.5")

  private def parseError(sample: String) = {
    validate(XML.loadString(sample)).asInstanceOf[Failure[_]].exception.toString
  }

  "DD-976-sample" should "reproduce the problems" in {
    val sample = File("src/test/resources/DD-976-provenance-validation/sample.xml").contentAsString
    val fixedDate = sample.replace("<changeDate>2014-01-01</changeDate>", "<changeDate>2014-01-01T13:13:20.776+02:00</changeDate>")
    val fixedBoth = fixedDate.replace("<prov:new>", """<prov:new xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/" xmlns:dcterms="http://purl.org/dc/terms/">""")
    assume(schemaIsAvailable)
    parseError(sample) shouldNot // schema is more flexible since https://github.com/DANS-KNAW/easy-schema/pull/104
      be("org.xml.sax.SAXParseException; lineNumber: 15; columnNumber: 56; cvc-datatype-valid.1.2.3: '2014-01-01' is not a valid value of union type 'dateTime-or-nothing'.")
    parseError(fixedDate) shouldBe
      """org.xml.sax.SAXParseException; lineNumber: 36; columnNumber: 68; The prefix "ddm" for element "ddm:language" is not bound."""
    validate(XML.loadString(fixedBoth)) shouldBe a[Success[_]]
  }

  "Provenance" should "show encoding changes" in {
    val ddmOut = XML.loadFile("src/test/resources/encoding/ddm-out.xml")
    val (ddmIn, oldChars, newChars) = loadXml(File("src/test/resources/encoding/ddm-in.xml"))
      .getOrElse(throw new IllegalArgumentException("could not load test data"))
    val provenance = provenanceBuilder.collectChangesInXmls(List(
        Provenance.fixedDdmEncoding(oldChars, newChars),
        Provenance.compareDDM(ddmIn, ddmOut),
      ))
    val expected = Utility.trim(XML.loadFile("src/test/resources/encoding/provenance.xml"))

    // CDATA is turned into escaped strings when comparing the full XML
    // trick does not work for funder test, perhaps because here we have multiple CDATA elements
    (provenance \\ "migration").map(Utility.trim) shouldBe (expected \\ "migration").map(Utility.trim)

    assume(schemaIsAvailable)
    validate(provenance) shouldBe a[Success[_]]
  }
  it should "show ddm diff" in {
    val ddmIn = {
      <ddm:DDM xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
               xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
               xmlns:dc="http://purl.org/dc/elements/1.1/"
               xmlns:dcterms="http://purl.org/dc/terms/"
               xmlns:dcx-gml="http://easy.dans.knaw.nl/schemas/dcx/gml/"
               xmlns:abr="http://www.den.nl/standaard/166/Archeologisch-Basisregister/"
               xsi:schemaLocation=" http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
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
      </ddm:DDM>
    }

    val ddmOut = {
      <ddm:DDM xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
               xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
               xmlns:dc="http://purl.org/dc/elements/1.1/"
               xmlns:dcterms="http://purl.org/dc/terms/"
               xmlns:dcx-gml="http://easy.dans.knaw.nl/schemas/dcx/gml/"
               xmlns:abr="http://www.den.nl/standaard/166/Archeologisch-Basisregister/"
               xsi:schemaLocation=" http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
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
          <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    }

    val expectedNew = Utility.trim(
            <prov:new>
              <ddm:reportNumber schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e" valueURI="https://data.cultureelerfgoed.nl/term/id/abr/fcff6035-9e90-450f-8b39-cf33447e6e9f" subjectScheme="ABR Rapporten" reportNo="456">Rapport 456</ddm:reportNumber>
              <ddm:reportNumber schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e" valueURI="https://data.cultureelerfgoed.nl/term/id/abr/90f3092a-818e-4db2-8467-35b64262c5b3" subjectScheme="ABR Rapporten" reportNo="2859">Transect-rapport 2859</ddm:reportNumber>
              <ddm:temporal xml:lang="nl" valueURI="https://data.cultureelerfgoed.nl/term/id/abr/330e7fe0-a1f7-43de-b448-d477898f6648" subjectScheme="ABR Perioden" schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/b6df7840-67bf-48bd-aa56-7ee39435d2ed">Vroege Middeleeuwen A</ddm:temporal>
              <ddm:subject xml:lang="nl" valueURI="https://data.cultureelerfgoed.nl/term/id/abr/6ae3ab19-49ca-44a7-8b65-3a3395014bb9" subjectScheme="ABR Complextypen" schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/b6df7840-67bf-48bd-aa56-7ee39435d2ed">veenwinning (inclusief zouthoudend veen t.b.v. zoutproductie)</ddm:subject>
              <ddm:subject xml:lang="nl" valueURI="https://data.cultureelerfgoed.nl/term/id/abr/f182d72c-2d22-47ae-b799-26dea01e770c" subjectScheme="ABR Complextypen" schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/b6df7840-67bf-48bd-aa56-7ee39435d2ed">akker / tuin</ddm:subject>
              <ddm:reportNumber schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e" valueURI="https://data.cultureelerfgoed.nl/term/id/abr/fcff6035-9e90-450f-8b39-cf33447e6e9f" subjectScheme="ABR Rapporten" reportNo="123">Rapport 123</ddm:reportNumber>
              <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
            </prov:new>
    )
    val expectedOld = """<dc:title xmlns:abr="http://www.den.nl/standaard/166/Archeologisch-Basisregister/" xmlns:dcx-gml="http://easy.dans.knaw.nl/schemas/dcx/gml/" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/">Rapport 456</dc:title>
                        |<dcterms:temporal xsi:type="abr:ABRperiode" xmlns:abr="http://www.den.nl/standaard/166/Archeologisch-Basisregister/" xmlns:dcx-gml="http://easy.dans.knaw.nl/schemas/dcx/gml/" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/">VMEA</dcterms:temporal>
                        |<dc:subject xsi:type="abr:ABRcomplex" xmlns:abr="http://www.den.nl/standaard/166/Archeologisch-Basisregister/" xmlns:dcx-gml="http://easy.dans.knaw.nl/schemas/dcx/gml/" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/">EGVW</dc:subject>
                        |<dcterms:subject xsi:type="abr:ABRcomplex" xmlns:abr="http://www.den.nl/standaard/166/Archeologisch-Basisregister/" xmlns:dcx-gml="http://easy.dans.knaw.nl/schemas/dcx/gml/" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/">ELA</dcterms:subject>""".stripMargin.mkString("")
    val provenance = provenanceBuilder.collectChangesInXmls(List(
      Provenance.compareDDM(ddmIn, ddmOut)
    ))
    (provenance \\ "old").text shouldBe expectedOld // might break when attributes are serialized in different order

    // replace is a hack for white space that should have been covered by normalized
    normalized((provenance \\ "new").head) shouldBe normalized(expectedNew)

    assume(schemaIsAvailable)
    val triedUnit = validate(provenance)
    triedUnit shouldBe a[Success[_]]
  }
  it should "show dropped zero point" in {
    val ddmIn = {
      <ddm:DDM xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
               xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
               xmlns:dc="http://purl.org/dc/elements/1.1/"
               xmlns:dcx-gml="http://easy.dans.knaw.nl/schemas/dcx/gml/"
               xsi:schemaLocation=" http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
        <ddm:profile>
          <dc:title>blabla</dc:title>
        </ddm:profile>
        <ddm:dcmiMetadata>
          <dcx-gml:spatial srsName="http://www.opengis.net/def/crs/EPSG/0/28992">
            <Point xmlns="http://www.opengis.net/gml">
              <pos>0 0</pos>
            </Point>
          </dcx-gml:spatial>
          <dcx-gml:spatial srsName="http://www.opengis.net/def/crs/EPSG/0/4326">
            <Point xmlns="http://www.opengis.net/gml">
              <pos></pos>
            </Point>
          </dcx-gml:spatial>
          <dcx-gml:spatial srsName="http://www.opengis.net/def/crs/EPSG/0/28992">
            <Point xmlns="http://www.opengis.net/gml">
              <description>Entrance of DANS Building</description>
              <name>Data Archiving and Networked Services (DANS)</name>
              <pos>0 0</pos>
            </Point>
          </dcx-gml:spatial>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    }

    val ddmOut = {
      <ddm xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
               xsi:schemaLocation=" http://easy.dans.knaw.nl/schemas/md/ddm/ https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd">
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
          <dcx-gml:spatial srsName="http://www.opengis.net/def/crs/EPSG/0/4326">
            <Point xmlns="http://www.opengis.net/gml">
              <pos></pos>
            </Point>
          </dcx-gml:spatial>
        </ddm:dcmiMetadata>
      </ddm>
    }

    val provenance = provenanceBuilder.collectChangesInXmls(List(
      Provenance.compareDDM(ddmIn, ddmOut)
    ))
    val expectedOld = """<dcx-gml:spatial srsName="http://www.opengis.net/def/crs/EPSG/0/28992" xmlns:dcx-gml="http://easy.dans.knaw.nl/schemas/dcx/gml/" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"><Point xmlns="http://www.opengis.net/gml"><pos>0 0</pos></Point></dcx-gml:spatial>"""
    (provenance \\ "old").text shouldBe expectedOld // might break when attributes are serialized in different order
    (provenance \\ "new").head.nonEmptyChildren shouldBe empty
    logger.trace(Utility.serialize(provenance, preserveWhitespace = true).toString())

    assume(schemaIsAvailable)
    validate(provenance) shouldBe a[Success[_]]
  }
  it should "keep valid spatial elements" in {
    val (ddmIn, _, _) = loadXml(File("src/test/resources/DD-858/dataset.xml"))
      .getOrElse(fail("could not load test data"))

    // a few steps of EasyConvertBagToDepositApp.addProps
    val ddmOut = testConfig("archaeology").ddmTransformer.transform(ddmIn, "easy-dataset:123")
      .getOrElse(fail("no DDM returned"))

    Provenance.compareDDM(ddmIn, ddmOut) shouldBe None

    assume(schemaIsAvailable)
    validate(ddmIn) shouldBe Success(())
  }
  it should "show funder diff" in {
    // compare the DDM files manually for finer details than in the provenance
    val ddmIn = XML.loadFile("src/test/resources/funder/ddm-in.xml")
    val ddmOut = XML.loadFile("src/test/resources/funder/ddm-out.xml")
    val expected = XML.loadFile("src/test/resources/funder/provenance.xml")

    val actual = provenanceBuilder.collectChangesInXmls(List(Provenance.compareDDM(ddmIn, ddmOut)))

    (Utility.trim(actual) \\ "old").text.replaceAll("\n","").replaceAll("><","> <") shouldBe
      (Utility.trim(expected) \\ "old").text // might break when attributes are serialized in different order
    normalized((actual \\ "new").head) shouldBe normalized((expected \\ "new").head)
    assume(schemaIsAvailable)
    validate(actual) shouldBe a[Success[_]]
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

    val transformer = new AmdTransformer(csvFile = File("src/main/assembly/dist/cfg/archaeology/account-substitutes.csv"))
    val amdIn = XML.loadFile((testDir / "amd.xml").toJava)
    val created = <ddm:created>2016-12-31</ddm:created>
    val amdOut = transformer.transform(amdIn, created).getOrElse(fail("could not transform"))
    amdOut.text shouldNot include("2017-05-02T13:01:26.752+02:00")
    amdOut.text should include("2016-12-31")
    val provenance = provenanceBuilder.collectChangesInXmls(List(
      Provenance.compareAMD(amdIn, amdOut)
    ))
    val expectedOld ="""<depositorId xmlns:damd="http://easy.dans.knaw.nl/easy/dataset-administrative-metadata/">user001</depositorId>
                       |<damd:stateChangeDate xmlns:damd="http://easy.dans.knaw.nl/easy/dataset-administrative-metadata/"><fromState>SUBMITTED</fromState><toState>PUBLISHED</toState><changeDate>2017-05-02T13:01:26.752+02:00</changeDate></damd:stateChangeDate>"""
      .stripMargin
    val expectedNew = Utility.trim(
            <prov:new>
              <depositorId>USer</depositorId>
              <damd:stateChangeDate xmlns:damd="http://easy.dans.knaw.nl/easy/dataset-administrative-metadata/">
                <fromState>SUBMITTED</fromState>
                <toState>PUBLISHED</toState>
                <changeDate>2016-12-31T00:00:00.000+01:00</changeDate>
              </damd:stateChangeDate>
            </prov:new>
    )
    (provenance \\ "old").text shouldBe expectedOld // might break when attributes are serialized in different order
    normalized((provenance \\ "new").head) shouldBe normalized(expectedNew)

    (provenance \\ "file").head.scope.toString() shouldBe """ xmlns:damd="http://easy.dans.knaw.nl/easy/dataset-administrative-metadata/""""
    logger.trace(Utility.serialize(x = provenance, preserveWhitespace = true).toString())

    assume(schemaIsAvailable)
    validate(provenance) shouldBe a[Success[_]]
  }

  it should "show replaced empty date in amd" in {
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
                <changeDate></changeDate>
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

    val (amdIn, _, _) = loadXml(testDir / "amd.xml").getOrElse(fail("could not load AMD"))
    val amdOut = new AmdTransformer(csvFile = File("src/main/assembly/dist/cfg/archaeology/account-substitutes.csv"))
      .transform(amdIn, <ddm:created>2016-12-31</ddm:created>)
      .getOrElse(fail("could not transform"))

    // post condition 1: date is added
    amdOut.text shouldNot include("<changeDate></changeDate>")
    amdOut.text should include("2016-12-31")
    val expected = Utility.trim{
      <prov:provenance xsi:schemaLocation={ schemaLocation } xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:prov="http://easy.dans.knaw.nl/schemas/bag/metadata/prov/">
        <prov:migration app="EasyConvertBagToDepositApp" version="1.0.5" date="2020-02-02">
          <prov:file scheme="http://easy.dans.knaw.nl/easy/dataset-administrative-metadata/">
            <prov:old><![CDATA[<depositorId xmlns:damd="http://easy.dans.knaw.nl/easy/dataset-administrative-metadata/">user001</depositorId>
<damd:stateChangeDate xmlns:damd="http://easy.dans.knaw.nl/easy/dataset-administrative-metadata/"><fromState>SUBMITTED</fromState><toState>PUBLISHED</toState><changeDate/></damd:stateChangeDate>]]></prov:old>
            <prov:new>
              <depositorId>USer</depositorId>
              <damd:stateChangeDate>
                <fromState>SUBMITTED</fromState>
                <toState>PUBLISHED</toState>
                <changeDate>2016-12-31T00:00:00.000+01:00</changeDate>
              </damd:stateChangeDate>
            </prov:new>
          </prov:file>
        </prov:migration>
      </prov:provenance>
    }

    // post condition 2: added date is reported in provenance
    val provenance = provenanceBuilder.collectChangesInXmls(List(
      Provenance.compareAMD(amdIn, amdOut),
    ))

    (provenance \\ "old").text shouldBe (expected \\ "old").text // might break when attributes are serialized in different order
    normalized((provenance \\ "new").head) shouldBe normalized((expected \\ "new").head)
    val actual = Utility.trim(provenance)
    actual.text shouldBe expected.text
    closingTags(actual) shouldBe closingTags(expected)

    (provenance \\ "file").head.scope.toString() shouldBe """ xmlns:damd="http://easy.dans.knaw.nl/easy/dataset-administrative-metadata/""""
    logger.trace(Utility.serialize(x = provenance, preserveWhitespace = true).toString())

    assume(schemaIsAvailable)
    validate(provenance) shouldBe a[Success[_]]
  }
}
