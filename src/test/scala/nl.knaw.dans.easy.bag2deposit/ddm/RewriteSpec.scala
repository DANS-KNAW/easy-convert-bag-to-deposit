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
import nl.knaw.dans.easy.bag2deposit.Fixture.SchemaSupport
import nl.knaw.dans.easy.bag2deposit.ddm.LanguageRewriteRule.logNotMappedLanguages
import nl.knaw.dans.easy.bag2deposit.{ Configuration, EasyConvertBagToDepositApp, InvalidBagException, normalized, parseCsv }
import org.apache.commons.csv.CSVRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.UUID
import scala.util.{ Failure, Success, Try }
import scala.xml.NodeBuffer

class RewriteSpec extends AnyFlatSpec with SchemaSupport with Matchers {
  private val cfgDir: File = File("src/main/assembly/dist/cfg")
  private val cfg = Configuration(cfgDir.parent)
  override val schema = "https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd"

  private val mandatoryInProfile =
          <dct:description>YYY</dct:description>
          <dcx-dai:creatorDetails>
            <dcx-dai:organization>
              <dcx-dai:name>DANS</dcx-dai:name>
            </dcx-dai:organization>
          </dcx-dai:creatorDetails>
          <ddm:created>2013-03</ddm:created>
          <ddm:available>2013-04</ddm:available>
          <ddm:audience>D37000</ddm:audience>
          <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>

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
    val ddmIn = ddm(
        <ddm:profile>
          <dc:title>Rapport 123</dc:title>
          { mandatoryInProfile }
        </ddm:profile>
        <ddm:dcmiMetadata>
            <dc:title>blabla</dc:title>
            <dc:title>Rapport 456</dc:title>
            <dcterms:temporal xsi:type="abr:ABRperiode">VMEA</dcterms:temporal>
            <dc:subject xsi:type="abr:ABRcomplex">EGVW</dc:subject>
            <dcterms:subject xsi:type="abr:ABRcomplex">ELA</dcterms:subject>
            <ddm:subject xml:lang="nl" valueURI="http://www.rnaproject.org/data/39a61516-5ebd-43ad-9cde-98b5089c71ff" subjectScheme="Archeologisch Basis Register" schemeURI="http://www.rnaproject.org">Onbekend (XXX)</ddm:subject>
        </ddm:dcmiMetadata>
    )

    val expectedDDM = ddm(
        <ddm:profile>
          <dc:title>Rapport 123</dc:title>
          { mandatoryInProfile }
        </ddm:profile>
        <ddm:dcmiMetadata>
            <ddm:reportNumber
              schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e"
              valueURI="https://data.cultureelerfgoed.nl/term/id/abr/fcff6035-9e90-450f-8b39-cf33447e6e9f"
              subjectScheme="ABR Rapporten"
              reportNo="123"
            >Rapport 123</ddm:reportNumber>
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
        </ddm:dcmiMetadata>
    )

    val app = new EasyConvertBagToDepositApp(cfg)

    // a few steps of EasyConvertBagToDepositApp.addPropsToBags
    val datasetId = "easy-dataset:123"
    cfg.ddmTransformer.transform(ddmIn, datasetId).map(normalized)
      .getOrElse(fail("no DDM returned")) shouldBe normalized(expectedDDM)
    app.registerMatchedReports(datasetId, expectedDDM \\ "reportNumber")
    app.logMatchedReports() // once for all datasets

    assume(schemaIsAvailable)
    validate(expectedDDM) shouldBe Success(())
  }

  "LanguageRewriteRule" should "convert" in {
    val ddmIn = ddm(
        <ddm:profile>
          <dc:title>language test</dc:title>
          { mandatoryInProfile }
        </ddm:profile>
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
    val expectedDDM = ddm(
        <ddm:profile>
          <dc:title>language test</dc:title>
          { mandatoryInProfile }
        </ddm:profile>
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
        </ddm:dcmiMetadata>
    )
    val datasetId = "eas-dataset:123"
    cfg.ddmTransformer.transform(ddmIn, datasetId).map(normalized)
      .getOrElse(fail("no DDM returned")) shouldBe normalized(expectedDDM)

    // TODO manually check logging of not mapped language fields
    logNotMappedLanguages(expectedDDM, datasetId)

    assume(schemaIsAvailable)
    validate(expectedDDM) shouldBe Success(())
  }

  it should "leave briefrapport untouched" in {
    val ddmIn = ddm(
        <ddm:profile>
          <dc:title>Briefrapport 123</dc:title>
          { mandatoryInProfile }
        </ddm:profile>
        <ddm:dcmiMetadata>
        </ddm:dcmiMetadata>
    )

    cfg.ddmTransformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe Success(normalized(ddmIn))
    // TODO manually check logging of briefrapport
  }

  it should "add inCollection" in {
    val ddmIn = ddm(
        <ddm:profile>
          <dc:title>blabla</dc:title>
          { mandatoryInProfile }
        </ddm:profile>
        <ddm:dcmiMetadata>
        </ddm:dcmiMetadata>
    )

    val transformer = cfg.ddmTransformer
      .copy(collectionsMap = Map("easy-dataset:123" -> <inCollection/>))
    transformer.transform(ddmIn, "easy-dataset:456").map(normalized) shouldBe Success(normalized(ddmIn))
    transformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe Success(normalized(ddm(
        <ddm:profile>
          <dc:title>blabla</dc:title>
          { mandatoryInProfile }
        </ddm:profile>
        <ddm:dcmiMetadata>
          <inCollection/>
        </ddm:dcmiMetadata>
    )))
  }

  it should "add report number of profile to dcmiMetadata" in {
    val ddmIn = ddm(
        <ddm:profile>
          <dc:title>Rapport 123: blablabla</dc:title>
          { mandatoryInProfile }
        </ddm:profile>
        <ddm:dcmiMetadata>
        </ddm:dcmiMetadata>
    )
    val expectedDdm = ddm(
        <ddm:profile>
          <dc:title>Rapport 123: blablabla</dc:title>
          { mandatoryInProfile }
        </ddm:profile>
        <ddm:dcmiMetadata>
          <ddm:reportNumber
            schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e"
            valueURI="https://data.cultureelerfgoed.nl/term/id/abr/fcff6035-9e90-450f-8b39-cf33447e6e9f"
            subjectScheme="ABR Rapporten"
            reportNo="123"
          >Rapport 123</ddm:reportNumber>
        </ddm:dcmiMetadata>
    )

    cfg.ddmTransformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
      Success(normalized(expectedDdm))
  }

  it should "add acquisition methods of profile to dcmiMetadata" in {
    val ddmIn = ddm(
        <ddm:profile>
          <dc:title>Een Inventariserend Veldonderzoek in de vorm van proefsleuven</dc:title>
          { mandatoryInProfile }
        </ddm:profile>
        <ddm:dcmiMetadata>
          <dc:title>Bureauonderzoek en Inventariserend veldonderzoek (verkennende fase)</dc:title>
        </ddm:dcmiMetadata>
    )
    val expectedDdm = ddm(
        <ddm:profile>
          <dc:title>Een Inventariserend Veldonderzoek in de vorm van proefsleuven</dc:title>
          { mandatoryInProfile }
        </ddm:profile>
        <ddm:dcmiMetadata>
              <ddm:acquisitionMethod
                schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/554ca1ec-3ed8-42d3-ae4b-47bcb848b238"
                valueURI={ s"https://data.cultureelerfgoed.nl/term/id/abr/a3354be9-15eb-4066-a4ec-40ed8895cb5a" }
                subjectScheme="ABR verwervingswijzen"
              >Een Inventariserend Veldonderzoek in de vorm van proefsleuven</ddm:acquisitionMethod>
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
        </ddm:dcmiMetadata>
    )

    cfg.ddmTransformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
      Success(normalized(expectedDdm))
  }

  it should "add multiple acquisition methods of profile to dcmiMetadata" in {
    val ddmIn = ddm(
        <ddm:profile>
          <dc:title>Archeologisch bureauonderzoek en gecombineerd verkennend en karterend booronderzoek</dc:title>
          { mandatoryInProfile }
        </ddm:profile>
        <ddm:dcmiMetadata>
        </ddm:dcmiMetadata>
    )
    val expectedDdm = ddm(
        <ddm:profile>
          <dc:title>Archeologisch bureauonderzoek en gecombineerd verkennend en karterend booronderzoek</dc:title>
          { mandatoryInProfile }
        </ddm:profile>
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
        </ddm:dcmiMetadata>
    )

    cfg.ddmTransformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
      Success(normalized(expectedDdm))
  }

  it should "complain about invalid period/subject" in {
    val ddmIn = ddm(
        <ddm:profile>
          <dc:title>blablabla</dc:title>
          { mandatoryInProfile }
        </ddm:profile>
        <ddm:dcmiMetadata>
            <dcterms:temporal xsi:type="abr:ABRperiode">rabarbera</dcterms:temporal>
            <dc:subject xsi:type="abr:ABRcomplex">barbapappa</dc:subject>
        </ddm:dcmiMetadata>
    )

    cfg.ddmTransformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
      Failure(InvalidBagException("temporal rabarbera not found; subject barbapappa not found"))
  }

  it should "match alkmaar variants" in {
    val ddmIn = ddm(
        <ddm:profile>
          <dc:title>blablabla</dc:title>
          { mandatoryInProfile }
        </ddm:profile>
        <ddm:dcmiMetadata>
            <dc:title>Rapporten over de Alkmaarse Monumenten en Archeologie 18</dc:title>
            <dc:title> RAMA 13</dc:title>
            <dc:title>Rapporten over de Alkmaarse Monumentenzorg en Archeologie RAMA 12</dc:title>
        </ddm:dcmiMetadata>
    )
    val expectedDdm = ddm(
        <ddm:profile>
          <dc:title>blablabla</dc:title>
          { mandatoryInProfile }
        </ddm:profile>
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
        </ddm:dcmiMetadata>
    )
    // TODO these titles don't show up in target/test/TitlesSpec/matches-per-rce.txt
    cfg.ddmTransformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
      Success(normalized(expectedDdm))
  }

  private def ddm(dcmi: NodeBuffer) =
      <ddm:DDM xmlns:dcx="http://easy.dans.knaw.nl/schemas/dcx/"
         xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xmlns:abr="http://www.den.nl/standaard/166/Archeologisch-Basisregister/"
         xmlns:dc="http://purl.org/dc/elements/1.1/"
         xmlns:dct="http://purl.org/dc/terms/"
         xmlns:dcterms="http://purl.org/dc/terms/"
         xmlns:dcx-dai="http://easy.dans.knaw.nl/schemas/dcx/dai/"
         xmlns:dcmitype="http://purl.org/dc/dcmitype/"
         xsi:schemaLocation="
         http://easy.dans.knaw.nl/schemas/md/ddm/ http://easy.dans.knaw.nl/schemas/md/2015/12/ddm.xsd
         http://www.den.nl/standaard/166/Archeologisch-Basisregister/ http://easy.dans.knaw.nl/schemas/vocab/2012/10/abr-type.xsd
         http://www.w3.org/2001/XMLSchema-instance http://easy.dans.knaw.nl/schemas/md/emd/2013/11/xml.xsd
         http://purl.org/dc/terms/ http://easy.dans.knaw.nl/schemas/emd/2013/11/qdc.xsd
         http://purl.org/dc/elements/1.1/ http://dublincore.org/schemas/xmls/qdc/dc.xsd
      "> { dcmi }
      </ddm:DDM>
}
