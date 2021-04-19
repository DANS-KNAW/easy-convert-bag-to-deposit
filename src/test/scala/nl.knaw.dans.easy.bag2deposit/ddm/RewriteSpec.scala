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
package nl.knaw.dans.easy.bag2deposit.ddm

import better.files.File
import nl.knaw.dans.easy.bag2deposit.Fixture.{ DdmSupport, SchemaSupport }
import nl.knaw.dans.easy.bag2deposit.ddm.LanguageRewriteRule.logNotMappedLanguages
import nl.knaw.dans.easy.bag2deposit.{ BagIndex, Configuration, EasyConvertBagToDepositApp, InvalidBagException, parseCsv }
import org.apache.commons.csv.CSVRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI
import java.nio.charset.Charset
import java.util.UUID
import scala.util.{ Failure, Success, Try }
import scala.xml.{ Node, PrettyPrinter, Utility }

class RewriteSpec extends AnyFlatSpec with SchemaSupport with Matchers with DdmSupport {
  private val cfgDir: File = File("src/main/assembly/dist/cfg")
  private val transformer: DdmTransformer = new DdmTransformer(cfgDir, Map.empty)

  override val schema = "https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd"

  "ABR-complex" should "be valid" in {
    val records = parseCsv(cfgDir / "ABR-complex.csv", AbrRewriteRule.nrOfHeaderLines)
    records.map(tryUuid).filter(_.isFailure) shouldBe empty
    getDuplicates(records) shouldBe empty
    records.size shouldBe AbrRewriteRule.subjectRewriteRule(cfgDir).map.size
  }

  "ABR-period" should "be valid" in {
    val records = parseCsv(cfgDir / "ABR-period.csv", AbrRewriteRule.nrOfHeaderLines)
    getDuplicates(records) shouldBe empty
    records.map(tryUuid).filter(_.isFailure) shouldBe empty
    records.size shouldBe AbrRewriteRule.temporalRewriteRule(cfgDir).map.size
  }

  private def tryUuid(r: CSVRecord) = Try(UUID.fromString(r.get(2)))

  private def getDuplicates(records: Iterable[CSVRecord]) = records.groupBy(_.get(0)).filter(_._2.size > 1)

  "ABR rules" should "convert" in {
    val ddmIn = ddm(title = "Rapport 123", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
            <dc:title>blabla</dc:title>
            <dc:title>Rapport 456</dc:title>
            <dcterms:temporal xsi:type="abr:ABRperiode">VMEA</dcterms:temporal>
            <dc:subject xsi:type="abr:ABRcomplex">EGVW</dc:subject>
            <dcterms:subject xsi:type="abr:ABRcomplex">ELA</dcterms:subject>
            <ddm:subject xml:lang="nl" valueURI="http://www.rnaproject.org/data/39a61516-5ebd-43ad-9cde-98b5089c71ff" subjectScheme="Archeologisch Basis Register" schemeURI="http://www.rnaproject.org">Onbekend (XXX)</ddm:subject>
            <ddm:subject xml:lang="nl"
                         valueURI="http://www.rnaproject.org/data/54f419f0-d185-4ea5-a188-57e25493a5e0"
                         subjectScheme="Archeologisch Basis Register"
                         schemeURI="http://www.rnaproject.org"
            >Religie - Klooster(complex) (RKLO)</ddm:subject>
        </ddm:dcmiMetadata>
    )

    val expectedDDM = ddm(title = "Rapport 123", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
            <dc:title>blabla</dc:title>
            <ddm:reportNumber
              schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e"
              valueURI="https://data.cultureelerfgoed.nl/term/id/abr/fcff6035-9e90-450f-8b39-cf33447e6e9f"
              subjectScheme="ABR Rapporten"
              reportNo="456"
            >Rapport 456</ddm:reportNumber>
            <ddm:temporal xml:lang="nl"
                          valueURI="https://data.cultureelerfgoed.nl/term/id/abr/330e7fe0-a1f7-43de-b448-d477898f6648"
                          subjectScheme="ABR Periodes"
                          schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/9b688754-1315-484b-9c89-8817e87c1e84"
            >Vroege Middeleeuwen A</ddm:temporal>
            <ddm:subject xml:lang="nl"
                         valueURI="https://data.cultureelerfgoed.nl/term/id/abr/6ae3ab19-49ca-44a7-8b65-3a3395014bb9"
                         subjectScheme="ABR Complextypen"
                         schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/e9546020-4b28-4819-b0c2-29e7c864c5c0"
            >veenwinning (inclusief zouthoudend veen t.b.v. zoutproductie)</ddm:subject>
            <ddm:subject xml:lang="nl"
                         valueURI="https://data.cultureelerfgoed.nl/term/id/abr/f182d72c-2d22-47ae-b799-26dea01e770c"
                         subjectScheme="ABR Complextypen"
                         schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/e9546020-4b28-4819-b0c2-29e7c864c5c0"
            >akker / tuin</ddm:subject>
            <ddm:subject xml:lang="nl"
                         valueURI="https://data.cultureelerfgoed.nl/term/id/abr/5fbda024-be3e-47ac-a6c8-1c58d2cf5ccc"
                         subjectScheme="ABR Complextypen"
                         schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/e9546020-4b28-4819-b0c2-29e7c864c5c0"
            >complextype niet te bepalen</ddm:subject>
            <ddm:subject xml:lang="nl"
                         valueURI="https://data.cultureelerfgoed.nl/term/id/abr/28e58033-875e-4f90-baa2-7b1c1c147574"
                         subjectScheme="ABR Complextypen"
                         schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/e9546020-4b28-4819-b0c2-29e7c864c5c0"
            >klooster</ddm:subject>
            <ddm:reportNumber
              schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e"
              valueURI="https://data.cultureelerfgoed.nl/term/id/abr/fcff6035-9e90-450f-8b39-cf33447e6e9f"
              subjectScheme="ABR Rapporten"
              reportNo="123"
            >Rapport 123</ddm:reportNumber>
            <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
        </ddm:dcmiMetadata>
    )

    val app = new EasyConvertBagToDepositApp(new Configuration(
      "test version",
      dansDoiPrefixes = "10.17026/,10.5072/".split(","),
      dataverseIdAuthority = "10.80270",
      bagIndex = BagIndex(new URI("http://localhost:20120/")),
      ddmTransformer = transformer,
    ))

    // a few steps of EasyConvertBagToDepositApp.addPropsToBags
    val datasetId = "easy-dataset:123"
    transformer.transform(ddmIn, datasetId).map(normalized)
      .getOrElse(fail("no DDM returned")) shouldBe normalized(expectedDDM)
    app.registerMatchedReports(datasetId, expectedDDM \\ "reportNumber")
    app.logMatchedReports() // once for all datasets

    assume(schemaIsAvailable)
    validate(expectedDDM) shouldBe Success(())
  }

  it should "complain about invalid period/subject" in {
    val ddmIn = ddm(title = "blablabla", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
            <dcterms:temporal xsi:type="abr:ABRperiode">rabarbera</dcterms:temporal>
            <dc:subject xsi:type="abr:ABRcomplex">barbapappa</dc:subject>
        </ddm:dcmiMetadata>
    )

    transformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
      Failure(InvalidBagException("temporal rabarbera not found; subject barbapappa not found"))
  }

  "languageRewriteRule" should "convert" in {
    val ddmIn = ddm(title = "language test", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
          <dcterms:language>in het Nederlands</dcterms:language>
          <dc:language>Engels</dc:language>
          <dct:language>nld</dct:language>
          <dc:language>ratjetoe</dc:language>
          <dc:language xsi:type='dcterms:ISO639-3'> huh</dc:language>
          <dc:language xsi:type='dcterms:ISO639-3'> nld </dc:language>
          <dc:language xsi:type='dcterms:ISO639-2'>ENG</dc:language>
          <dcterms:language xsi:type='dcterms:ISO639-3'>deu</dcterms:language>
          <dcterms:language xsi:type='dcterms:ISO639-2'>fra</dcterms:language>
        </ddm:dcmiMetadata>
    )
    val expectedDDM = ddm(title = "language test", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
          <ddm:language encodingScheme="ISO639-2" code="dut">in het Nederlands</ddm:language>
          <ddm:language encodingScheme="ISO639-2" code="eng">Engels</ddm:language>
          <ddm:language encodingScheme="ISO639-2" code="dut">nld</ddm:language>
          <dc:language>ratjetoe</dc:language>
          <dc:language xsi:type='dcterms:ISO639-3'> huh</dc:language>
          <ddm:language encodingScheme='ISO639-2' code="dut">Dutch</ddm:language>
          <ddm:language encodingScheme="ISO639-2" code="eng">English</ddm:language>
          <ddm:language encodingScheme='ISO639-2' code="ger">German</ddm:language>
          <ddm:language encodingScheme='ISO639-2' code="fre">French</ddm:language>
          <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
        </ddm:dcmiMetadata>
    )
    val datasetId = "eas-dataset:123"
    transformer.transform(ddmIn, datasetId).map(normalized)
      .getOrElse(fail("no DDM returned")) shouldBe normalized(expectedDDM)

    // TODO manually check logging of not mapped language fields
    logNotMappedLanguages(expectedDDM, datasetId)

    assume(schemaIsAvailable)
    validate(expectedDDM) shouldBe Success(())
  }

  "reportRewriteRule" should "leave briefrapport untouched" in {
    val ddmIn = ddm(title = "Briefrapport 123", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
        </ddm:dcmiMetadata>
    )
    val ddmExpected = ddm(title = "Briefrapport 123", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
        <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
        </ddm:dcmiMetadata>
    )

    transformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe Success(normalized(ddmExpected))
    // TODO manually check logging of briefrapport
  }

  it should "add report number of profile to dcmiMetadata" in {
    val ddmIn = ddm(title = "Rapport 123: blablabla", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
        </ddm:dcmiMetadata>
    )
    val expectedDdm = ddm(title = "Rapport 123: blablabla", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
          <ddm:reportNumber
            schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e"
            valueURI="https://data.cultureelerfgoed.nl/term/id/abr/fcff6035-9e90-450f-8b39-cf33447e6e9f"
            subjectScheme="ABR Rapporten"
            reportNo="123"
          >Rapport 123</ddm:reportNumber>
          <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
        </ddm:dcmiMetadata>
    )

    transformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
      Success(normalized(expectedDdm))
  }

  it should "match Alkmaar variants" in {
    val ddmIn = ddm(title = "blablabla", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
            <dc:title>Rapporten over de Alkmaarse Monumenten en Archeologie 18</dc:title>
            <dc:title> RAMA 13</dc:title>
            <dc:title>Rapporten over de Alkmaarse Monumentenzorg en Archeologie RAMA 12</dc:title>
        </ddm:dcmiMetadata>
    )
    val expectedDdm = ddm(title = "blablabla", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
            <ddm:reportNumber schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e"
                              valueURI="https://data.cultureelerfgoed.nl/term/id/abr/05c754af-7944-4971-8280-9e1b4e474a8d"
                              subjectScheme="ABR Rapporten" reportNo="18">
              Rapporten over de Alkmaarse Monumenten en Archeologie 18
            </ddm:reportNumber>
            <ddm:reportNumber schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e"
                              valueURI="https://data.cultureelerfgoed.nl/term/id/abr/05c754af-7944-4971-8280-9e1b4e474a8d"
                              subjectScheme="ABR Rapporten" reportNo="13">
              RAMA 13
            </ddm:reportNumber>
            <ddm:reportNumber schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e"
                              valueURI="https://data.cultureelerfgoed.nl/term/id/abr/05c754af-7944-4971-8280-9e1b4e474a8d"
                              subjectScheme="ABR Rapporten" reportNo="12">
              Rapporten over de Alkmaarse Monumentenzorg en Archeologie RAMA 12
            </ddm:reportNumber>
            <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
        </ddm:dcmiMetadata>
    )
    // TODO these titles don't show up in target/test/TitlesSpec/matches-per-rce.txt
    transformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
      Success(normalized(expectedDdm))
  }

  "acquisitionRewriteRule" should "add acquisition methods of profile to dcmiMetadata" in {
    val ddmIn = ddm(title = "Een Inventariserend Veldonderzoek in de vorm van proefsleuven", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
          <dc:title>Bureauonderzoek en Inventariserend veldonderzoek (verkennende fase)</dc:title>
        </ddm:dcmiMetadata>
    )
    val expectedDdm = ddm(title = "Een Inventariserend Veldonderzoek in de vorm van proefsleuven", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
              <ddm:acquisitionMethod
                schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/554ca1ec-3ed8-42d3-ae4b-47bcb848b238"
                valueURI={ s"https://data.cultureelerfgoed.nl/term/id/abr/d4ecc89b-d52e-49a1-880a-296db5c2953e" }
                subjectScheme="ABR verwervingswijzen"
              >Bureauonderzoek en Inventariserend veldonderzoek (verkennende fase)</ddm:acquisitionMethod>
              <ddm:acquisitionMethod
                schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/554ca1ec-3ed8-42d3-ae4b-47bcb848b238"
                valueURI={ s"https://data.cultureelerfgoed.nl/term/id/abr/bd4d913f-1cab-4f08-ab00-77b64a6273e0" }
                subjectScheme="ABR verwervingswijzen"
              >Bureauonderzoek en Inventariserend veldonderzoek (verkennende fase)</ddm:acquisitionMethod>
              <ddm:acquisitionMethod
                schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/554ca1ec-3ed8-42d3-ae4b-47bcb848b238"
                valueURI={ s"https://data.cultureelerfgoed.nl/term/id/abr/a3354be9-15eb-4066-a4ec-40ed8895cb5a" }
                subjectScheme="ABR verwervingswijzen"
              >Een Inventariserend Veldonderzoek in de vorm van proefsleuven</ddm:acquisitionMethod>
              <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
        </ddm:dcmiMetadata>
    )

    transformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
      Success(normalized(expectedDdm))
  }

  it should "add multiple acquisition methods of profile to dcmiMetadata" in {
    val ddmIn = ddm(title = "Archeologisch bureauonderzoek en gecombineerd verkennend en karterend booronderzoek", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
        </ddm:dcmiMetadata>
    )
    val expectedDdm = ddm(title = "Archeologisch bureauonderzoek en gecombineerd verkennend en karterend booronderzoek", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
              <ddm:acquisitionMethod
                schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/554ca1ec-3ed8-42d3-ae4b-47bcb848b238"
                valueURI={ s"https://data.cultureelerfgoed.nl/term/id/abr/d4ecc89b-d52e-49a1-880a-296db5c2953e" }
                subjectScheme="ABR verwervingswijzen"
              >Archeologisch bureauonderzoek en gecombineerd verkennend en karterend booronderzoek</ddm:acquisitionMethod>
              <ddm:acquisitionMethod
                schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/554ca1ec-3ed8-42d3-ae4b-47bcb848b238"
                valueURI={ s"https://data.cultureelerfgoed.nl/term/id/abr/bd4d913f-1cab-4f08-ab00-77b64a6273e0" }
                subjectScheme="ABR verwervingswijzen"
              >Archeologisch bureauonderzoek en gecombineerd verkennend en karterend booronderzoek</ddm:acquisitionMethod>
              <ddm:acquisitionMethod
                schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/554ca1ec-3ed8-42d3-ae4b-47bcb848b238"
                valueURI={ s"https://data.cultureelerfgoed.nl/term/id/abr/0bd089db-adf3-4246-8828-e64224e2324b" }
                subjectScheme="ABR verwervingswijzen"
              >Archeologisch bureauonderzoek en gecombineerd verkennend en karterend booronderzoek</ddm:acquisitionMethod>
              <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
        </ddm:dcmiMetadata>
    )

    transformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
      Success(normalized(expectedDdm))
  }

  "ddmTransformer" should "add inCollection for archaeology" in {
    val profile = <dc:title>blabla</dc:title><dct:description/> +: creator +: created +: available +: archaeology +: openAccess
    val ddmIn = ddm(
      <ddm:profile>{ profile }</ddm:profile>
      <ddm:dcmiMetadata/>
    )
    val transformer = new DdmTransformer(
      cfgDir,
      Map("easy-dataset:123" -> <inCollection>mocked</inCollection>)
    )

    transformer.transform(ddmIn, "easy-dataset:456").map(normalized) shouldBe Success(normalized(ddm(
      <ddm:profile>{ profile }</ddm:profile>
        <ddm:dcmiMetadata>
          <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
        </ddm:dcmiMetadata>
    )))
    transformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe Success(normalized(
      ddm(
        <ddm:profile>{ profile }</ddm:profile>
        <ddm:dcmiMetadata>
          <inCollection>mocked</inCollection>
          <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
        </ddm:dcmiMetadata>
      )))
    // content of the <inCollection> element is validated in CollectionsSpec.collectionDatasetIdToInCollection
  }

  it should "add inCollection to an empty dcmiMetadata for other than archaeology" in {
    val profile = <dc:title>rabarbera</dc:title><dct:description/> +: creator +: created +: available +: <ddm:audience>Z99000</ddm:audience> +: openAccess
    val ddmIn = ddm(
      <ddm:profile>{ profile }</ddm:profile>
      <ddm:dcmiMetadata/>
    )
    val transformer = new DdmTransformer(
      cfgDir,
      Map("easy-dataset:123" -> <inCollection>mocked</inCollection>)
    )

    transformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe Success(normalized(
      ddm(
        <ddm:profile>{ profile }</ddm:profile>
        <ddm:dcmiMetadata>
          <inCollection>mocked</inCollection>
          <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
        </ddm:dcmiMetadata>
      )))
  }
  it should "add inCollection and filter titles" in {
    val profile = <dc:title>blabla rabarbera</dc:title><dct:description/> +: creator +: created +: available +: <ddm:audience>Z99000</ddm:audience> +: openAccess
    val ddmIn = ddm(
        <ddm:profile>{ profile }</ddm:profile>
        <ddm:dcmiMetadata>
          <dct:alternative>blabla</dct:alternative>
          <dct:alternative>rabarbera</dct:alternative>
          <dct:alternative>asterix</dct:alternative>
          <dct:alternative>asterix</dct:alternative>
          <dc:title>asterix en obelix</dc:title>
          <dct:alternative>blabla rabarbera ratjetoe</dct:alternative>
        </ddm:dcmiMetadata>
    )
    val transformer = new DdmTransformer(
      cfgDir,
      Map("easy-dataset:123" -> <inCollection>mocked</inCollection>)
    )

    transformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe Success(normalized(
      ddm(
        <ddm:profile>{ profile }</ddm:profile>
        <ddm:dcmiMetadata>
          <dc:title>asterix en obelix</dc:title>
          <dct:alternative>blabla rabarbera ratjetoe</dct:alternative>
          <inCollection>mocked</inCollection>
          <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
        </ddm:dcmiMetadata>
      )))
  }
  it should "add rightsHolder with proper prefix" in {
    val profile = <dc:title>blabla rabarbera</dc:title><dct:description/> +: creator +: created +: available +: <ddm:audience>Z99000</ddm:audience> +: openAccess
    // note that ddm in other tests have both dct as dcterms as namespace prefix for the same URI
    val ddmIn = {
      <ddm:DDM xmlns:dct="http://purl.org/dc/terms/">
        <ddm:dcmiMetadata/>
      </ddm:DDM>
    }
    val ddmExpected = {
      <ddm:DDM xmlns:dct="http://purl.org/dc/terms/">
        <ddm:dcmiMetadata>
          <dct:rightsHolder>Unknown</dct:rightsHolder>
        </ddm:dcmiMetadata>
      </ddm:DDM>
    }
    val transformer = new DdmTransformer(cfgDir, Map.empty)
    transformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
      Success(normalized(ddmExpected))
  }
  it should "recognize rightsHolder" in {
    val profile = <dc:title>blabla rabarbera</dc:title><dct:description/> +: creator +: created +: available +: <ddm:audience>Z99000</ddm:audience> +: openAccess
    val ddmIn = ddm(
        <ddm:profile>{ profile }</ddm:profile>
        <ddm:dcmiMetadata>
          <dct:rightsHolder>Some Body</dct:rightsHolder>
        </ddm:dcmiMetadata>
    )
    val transformer = new DdmTransformer(cfgDir, Map.empty)
    transformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
      Success(normalized(ddmIn))
  }
  it should "recognize rightsHolder in a role" in {
    val profile = <dc:title>blabla rabarbera</dc:title><dct:description/> +: creator +: created +: available +: <ddm:audience>Z99000</ddm:audience> +: openAccess
    val ddmIn = ddm(
        <ddm:profile>{ profile }</ddm:profile>
        <ddm:dcmiMetadata>
          <dcx-dai:contributorDetails>
            <dcx-dai:organization>
              <dcx-dai:name>DANS</dcx-dai:name>
              <dcx-dai:role>RightsHolder</dcx-dai:role>
            </dcx-dai:organization>
          </dcx-dai:contributorDetails>
        </ddm:dcmiMetadata>
    )
    val transformer = new DdmTransformer(cfgDir, Map.empty)
    transformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
      Success(normalized(ddmIn))
  }
  it should "split archis nrs" in {
    val ddmIn = ddm(title = "blabla", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
          <dct:identifier scheme="blabla">411047; 411049; 411050;  (Archis-vondstmeldingsnr.)</dct:identifier>
          <dct:identifier>52427; 52429; 52431; 52433; 52435; 52437; 52439; 52441; 52462 (RAAP) (Archis waarneming)</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-WAARNEMING">441832; 1234</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-ZAAK-IDENTIFICATIE">567; 89</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-VONDSTMELDING">1011; 1213</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-MONUMENT">1415; 1617</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-ONDERZOEK">443456; 789; </dct:identifier>
        </ddm:dcmiMetadata>
    )
    val transformer = new DdmTransformer(cfgDir, Map.empty)

    transformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe Success(normalized(
      ddm(title = "blabla", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
          <dct:identifier xsi:type="id-type:ARCHIS-VONDSTMELDING" scheme="blabla">411047</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-VONDSTMELDING" scheme="blabla">411049</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-VONDSTMELDING" scheme="blabla">411050</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-WAARNEMING">52427</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-WAARNEMING">52429</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-WAARNEMING">52431</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-WAARNEMING">52433</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-WAARNEMING">52435</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-WAARNEMING">52437</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-WAARNEMING">52439</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-WAARNEMING">52441</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-WAARNEMING">52462</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-WAARNEMING">441832</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-WAARNEMING">1234</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-ZAAK-IDENTIFICATIE">567</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-ZAAK-IDENTIFICATIE">89</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-VONDSTMELDING">1011</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-VONDSTMELDING">1213</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-MONUMENT">1415</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-MONUMENT">1617</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-ONDERZOEK">443456</dct:identifier>
          <dct:identifier xsi:type="id-type:ARCHIS-ONDERZOEK">789</dct:identifier>
          <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
        </ddm:dcmiMetadata>
      )))
  }
  it should "create type-id from archis description" in {
    val transformer = new DdmTransformer(cfgDir, Map.empty)
    val archisIds = File("src/test/resources/possibleArchaeologyIdentifiers.txt")
      .lines(Charset.forName("UTF-8"))
      .filter(_.toLowerCase.contains("archis"))
      .map(id => <dct:identifier>{id}</dct:identifier>)
    val ddmIn = ddm(title = "blabla", audience = "D37000", dcmi =
          <ddm:dcmiMetadata>{ archisIds }</ddm:dcmiMetadata>
    )
    val triedNode = transformer.transform(ddmIn, "easy-dataset:123")
    val ddmOut = triedNode.getOrElse(fail("not expecting a conversion failure"))
    val strings = (ddmOut \\ "identifier").map(_.text)
    archisIds.size shouldNot be(strings.size)
    strings.filter(_.matches(".*[^0-9].*")) shouldBe Seq("10HZ-18 (Objectcode Archis)", "36141 (ARCHIS rapportnummer)", " 405800 (Archis nummers)", "http://livelink.archis.nl/Livelink/livelink.exe?func=ll&objId=4835986&objAction=browse (URI)", "66510 (Archisnummer)", "ARCHIS2: 63389", "Onderzoeksnaam Archis: 4042 Den Haag", "Objectnummer Archis: 1121031", "Archis2 nummer 65495", "3736 (RAAP) (Archis art. 41)", "6663 (ADC) (Archis art. 41)", "2866 (RAAP) (Archis art. 41)", "7104 (ADC) (Archis art. 41)", "16065 (BeVdG) (Archis art. 41)", "Archis2: CIS-code: 25499 (Tjeppenboer) en 25500 (Hilaard)")
  }
  it should "drop empty relation" in {
    val ddmIn = ddm(title = "blabla", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
          <dct:isFormatOf scheme="blabla"></dct:isFormatOf>
          <ddm:isRequiredBy href="http://does.not.exist.dans.knaw.nl"></ddm:isRequiredBy>
        </ddm:dcmiMetadata>
    )
    val transformer = new DdmTransformer(
      cfgDir,
      Map.empty,
    )

    transformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
      Success(normalized(ddm(
        title = "blabla",
        audience = "D37000",
        dcmi = <ddm:dcmiMetadata>
                 <ddm:isRequiredBy href="http://does.not.exist.dans.knaw.nl">http://does.not.exist.dans.knaw.nl</ddm:isRequiredBy>
                 <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
               </ddm:dcmiMetadata>,
      )))
  }

  private val nameSpaceRegExp = """ xmlns:[a-z-]+="[^"]*"""" // these attributes have a variable order
  private val printer = new PrettyPrinter(160, 2) // Utility.serialize would preserve white space, now tests are better readable

  def normalized(elem: Node): String = printer
    .format(Utility.trim(elem)) // this trim normalizes <a/> and <a></a>
    .replaceAll(nameSpaceRegExp, "") // the random order would cause differences in actual and expected
    .replaceAll(" +\n?", " ")
    .replaceAll("\n +<", "\n<")
    .trim
}
