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
import nl.knaw.dans.easy.bag2deposit.ddm.LanguageRewriteRule.logNotMappedLanguages
import nl.knaw.dans.easy.bag2deposit.{ InvalidBagException, loadXml, parseCsv }
import org.apache.commons.csv.CSVRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.Charset
import java.util.UUID
import scala.util.{ Failure, Success, Try }
import scala.xml.{ NodeBuffer, Utility, XML }

class RewriteSpec extends AnyFlatSpec with XmlSupport with SchemaSupport with Matchers with DdmSupport with FileSystemSupport with AppConfigSupport {
  private val cfgDir = File("src/main/assembly/dist/cfg")
  private val archaeologyCfgDir: File = cfgDir / "archaeology"

  override val schema = "https://easy.dans.knaw.nl/schemas/md/ddm/ddm.xsd"

  "ABR-complex" should "be valid" in {
    val records = parseCsv(archaeologyCfgDir / "ABR-complex.csv", AbrRewriteRule.nrOfHeaderLines)
    records.map(tryUuid).filter(_.isFailure) shouldBe empty
    getDuplicates(records) shouldBe empty
    records.size shouldBe AbrRewriteRule.subjectRewriteRule(archaeologyCfgDir).map.size
  }

  "ABR-period" should "be valid" in {
    val records = parseCsv(archaeologyCfgDir / "ABR-period.csv", AbrRewriteRule.nrOfHeaderLines)
    getDuplicates(records) shouldBe empty
    records.map(tryUuid).filter(_.isFailure) shouldBe empty
    records.size shouldBe AbrRewriteRule.temporalRewriteRule(archaeologyCfgDir).map.size
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
            <ddm:temporal xml:lang="nld"
                          valueURI="https://data.cultureelerfgoed.nl/term/id/abr/330e7fe0-a1f7-43de-b448-d477898f6648"
                          subjectScheme="ABR Periodes"
                          schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/9b688754-1315-484b-9c89-8817e87c1e84"
            >Vroege Middeleeuwen A</ddm:temporal>
            <ddm:subject xml:lang="nld"
                         valueURI="https://data.cultureelerfgoed.nl/term/id/abr/6ae3ab19-49ca-44a7-8b65-3a3395014bb9"
                         subjectScheme="ABR Complextypen"
                         schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/e9546020-4b28-4819-b0c2-29e7c864c5c0"
            >veenwinning (inclusief zouthoudend veen t.b.v. zoutproductie)</ddm:subject>
            <ddm:subject xml:lang="nld"
                         valueURI="https://data.cultureelerfgoed.nl/term/id/abr/f182d72c-2d22-47ae-b799-26dea01e770c"
                         subjectScheme="ABR Complextypen"
                         schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/e9546020-4b28-4819-b0c2-29e7c864c5c0"
            >akker / tuin</ddm:subject>
            <ddm:subject xml:lang="nld"
                         valueURI="https://data.cultureelerfgoed.nl/term/id/abr/5fbda024-be3e-47ac-a6c8-1c58d2cf5ccc"
                         subjectScheme="ABR Complextypen"
                         schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/e9546020-4b28-4819-b0c2-29e7c864c5c0"
            >complextype niet te bepalen</ddm:subject>
            <ddm:subject xml:lang="nld"
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

    // a few steps of EasyConvertBagToDepositApp.addPropsToBags
    val datasetId = "easy-dataset:123"
    testConfig("archaeology").ddmTransformer.transform(ddmIn, datasetId).map(normalized)
      .getOrElse(fail("no DDM returned")) shouldBe normalized(expectedDDM)

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

    testConfig("archaeology").ddmTransformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
      Failure(InvalidBagException("temporal rabarbera not found; subject barbapappa not found"))
  }

  "relationRewriteRule" should "convert encoded fedora-ids to DOI" in {
    //
    val ddmIn = ddm(title = "relation test", audience = "D37000", dcmi =
      <ddm:dcmiMetadata>
        <ddm:references
          href="https%3A%2F%2Feasy.dans.knaw.nl%2Fui%2Fdatasets%2Fid%2Feasy-dataset%3A48786">
          Plangebied Harinxmaland, gemeente Sneek. Archeologisch vooronderzoek: een inventariserend veldonderzoek (waarderend onderzoek)
        </ddm:references>
        <ddm:isReferencedBy
          href="https://easy.dans.knaw.nl/ui/datasets/id/easy-dataset:56023">
        </ddm:isReferencedBy>
        <ddm:references>https://easy.dans.knaw.nl/ui/datasets/id/easy-dataset:48515</ddm:references>
        <dcterms:references>https://easy.dans.knaw.nl/ui/datasets/id/easy-dataset:56024</dcterms:references>
        <dcterms:references>persistent-identifier.nl/?identifier=urn%3Anbn%3Anl%3Aui%3A13-08dh-73</dcterms:references>
        <ddm:references href="urn%3Anbn%3Anl%3Aui%3A13-04r-39p">rabarbera</ddm:references>
      </ddm:dcmiMetadata>
    )
    val expectedDDM = ddm(title = "relation test", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
          <ddm:references
            scheme="id-type:DOI"
            href="https://doi.org/10.17026/dans-zwe-6qtu">
            Plangebied Harinxmaland, gemeente Sneek. Archeologisch vooronderzoek: een inventariserend veldonderzoek (waarderend onderzoek)
          </ddm:references>
          <ddm:isReferencedBy
            scheme="id-type:DOI"
            href="https://doi.org/10.17026/dans-267-2y8q">
            https://doi.org/10.17026/dans-267-2y8q
          </ddm:isReferencedBy>
          <ddm:references
            scheme="id-type:DOI"
            href="https://doi.org/10.17026/dans-xc4-vj4h">
            https://doi.org/10.17026/dans-xc4-vj4h
          </ddm:references>
          <dcterms:references>https://doi.org/10.17026/dans-xpg-j2f6</dcterms:references>
          <dcterms:references>https://doi.org/10.17026/dans-z2s-vnu8</dcterms:references>
          <ddm:references
            scheme="id-type:DOI"
            href="https://doi.org/10.17026/dans-x7e-9m6k">
            rabarbera
          </ddm:references>
          <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
        </ddm:dcmiMetadata>
    )
    val datasetId = "easy-dataset:123"
    testConfig("archaeology").ddmTransformer.transform(ddmIn, datasetId).map(normalized)
      .getOrElse(fail("no DDM returned")) shouldBe normalized(expectedDDM)

    assume(schemaIsAvailable)
    validate(expectedDDM) shouldBe Success(())
  }
  it should "only convert links to the landing page (DD-1127)" in {
    //
    val ddmIn = ddm(title = "relation test", audience = "D37000", dcmi =
      <ddm:dcmiMetadata>
        <ddm:references
          href="https://easy.dans.knaw.nl/ui/datasets/id/easy-dataset:48786">
          Plangebied Harinxmaland, gemeente Sneek. Archeologisch vooronderzoek: een inventariserend veldonderzoek (waarderend onderzoek)
        </ddm:references>
        <ddm:isReferencedBy
          href="https://easy.dans.knaw.nl/ui/datasets/id/easy-dataset:56023">
        </ddm:isReferencedBy>
        <ddm:references>https://easy.dans.knaw.nl/ui/datasets/id/easy-dataset:48515</ddm:references>
        <dcterms:references>https://easy.dans.knaw.nl/ui/datasets/id/easy-dataset:56024</dcterms:references>
        <dcterms:references>persistent-identifier.nl/?identifier=urn:nbn:nl:ui:13-08dh-73</dcterms:references>
        <dcterms:references>http://datareviews.dans.knaw.nl/details.php?pid=urn:nbn:nl:ui:13-l9l-0gx</dcterms:references>
        <ddm:references href="urn:nbn:nl:ui:13-04r-39p">rabarbera</ddm:references>
      </ddm:dcmiMetadata>
    )
    val expectedDDM = ddm(title = "relation test", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
          <ddm:references
            scheme="id-type:DOI"
            href="https://doi.org/10.17026/dans-zwe-6qtu">
            Plangebied Harinxmaland, gemeente Sneek. Archeologisch vooronderzoek: een inventariserend veldonderzoek (waarderend onderzoek)
          </ddm:references>
          <ddm:isReferencedBy
            scheme="id-type:DOI"
            href="https://doi.org/10.17026/dans-267-2y8q">
            https://doi.org/10.17026/dans-267-2y8q
          </ddm:isReferencedBy>
          <ddm:references
            scheme="id-type:DOI"
            href="https://doi.org/10.17026/dans-xc4-vj4h">
            https://doi.org/10.17026/dans-xc4-vj4h
          </ddm:references>
          <dcterms:references>https://doi.org/10.17026/dans-xpg-j2f6</dcterms:references>
          <dcterms:references>https://doi.org/10.17026/dans-z2s-vnu8</dcterms:references>
          <dcterms:references>http://datareviews.dans.knaw.nl/details.php?pid=urn:nbn:nl:ui:13-l9l-0gx</dcterms:references>
          <ddm:references
            scheme="id-type:DOI"
            href="https://doi.org/10.17026/dans-x7e-9m6k">
            rabarbera
          </ddm:references>
          <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
        </ddm:dcmiMetadata>
    )
    val datasetId = "easy-dataset:123"
    testConfig("archaeology").ddmTransformer.transform(ddmIn, datasetId).map(normalized)
      .getOrElse(fail("no DDM returned")) shouldBe normalized(expectedDDM)

    assume(schemaIsAvailable)
    validate(expectedDDM) shouldBe Success(())
  }
  it should "drop empty relation" in {
    val ddmIn = ddm(title = "blabla", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
          <dct:isFormatOf scheme="blabla"></dct:isFormatOf>
          <ddm:references scheme="blabla"></ddm:references>
          <ddm:isRequiredBy href="http://does.not.exist.dans.knaw.nl"></ddm:isRequiredBy>
          <ddm:relation>https://blablabla</ddm:relation>
          <dcterms:relation>https://rabarbera</dcterms:relation>
        </ddm:dcmiMetadata>
    )

    testConfig("archaeology").ddmTransformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
      Success(normalized(ddm(
        title = "blabla",
        audience = "D37000",
        dcmi = <ddm:dcmiMetadata>
                 <ddm:isRequiredBy href="http://does.not.exist.dans.knaw.nl">http://does.not.exist.dans.knaw.nl</ddm:isRequiredBy>
                 <ddm:relation href="https://blablabla">https://blablabla</ddm:relation>
                 <dcterms:relation>https://rabarbera</dcterms:relation>
                 <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
               </ddm:dcmiMetadata>,
      )))
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
    testConfig("archaeology").ddmTransformer.transform(ddmIn, datasetId).map(normalized)
      .getOrElse(fail("no DDM returned")) shouldBe normalized(expectedDDM)

    // TODO manually check logging of not mapped language fields
    logNotMappedLanguages(expectedDDM, datasetId)

    assume(schemaIsAvailable)
    validate(expectedDDM) shouldBe Success(())
  }

  "dates" should "should only extend in the profile in both transformers" in {
    def profile (dates: NodeBuffer) = <dc:title>RAMA 10</dc:title><dct:description/> +: creator +: dates +: <ddm:audience>D11200</ddm:audience> +: openAccess
    val inputProfile = profile(<ddm:created>2013</ddm:created><ddm:available>2017-03</ddm:available>)
    val expectedProfile = profile(<ddm:created>2013-01-01</ddm:created><ddm:available>2017-03-01</ddm:available>)
    val inputDDM = ddm(
      <ddm:profile>{ inputProfile }</ddm:profile>
      <ddm:dcmiMetadata>
        <ddm:created>2012</ddm:created>
      </ddm:dcmiMetadata>
    )
    val expectedStandardDDM = ddm(
      <ddm:profile>{ expectedProfile }</ddm:profile>
      <ddm:dcmiMetadata>
        <ddm:created>2012</ddm:created>
        <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
      </ddm:dcmiMetadata>
    )
    val expectedArchaeologyDDM = ddm(
      <ddm:profile>{ expectedProfile }</ddm:profile>
      <ddm:dcmiMetadata>
        <ddm:created>2012</ddm:created>
        <ddm:reportNumber schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e"
                          valueURI="https://data.cultureelerfgoed.nl/term/id/abr/05c754af-7944-4971-8280-9e1b4e474a8d"
                          subjectScheme="ABR Rapporten" reportNo="10">
          RAMA 10
        </ddm:reportNumber>
        <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
      </ddm:dcmiMetadata>
    )

    testConfig("SSH").ddmTransformer.transform(inputDDM, "eas-dataset:123").map(normalized)
      .getOrElse(fail("no DDM returned")) shouldBe normalized(expectedStandardDDM)
    testConfig("archaeology").ddmTransformer.transform(inputDDM, "eas-dataset:123").map(normalized)
      .getOrElse(fail("no DDM returned")) shouldBe normalized(expectedArchaeologyDDM)
  }

  "datesOfCollection" should "convert a proper dates pair" in {
    val ddmIn = ddm(title = "datesOfCollection  test", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
          <dc:date>2019-10-20 (startdatum)</dc:date>
          <dc:date>2019-10-24 (einddatum)</dc:date>
        </ddm:dcmiMetadata>
    )
    val expectedDDM = ddm(title = "datesOfCollection test", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
          <ddm:datesOfCollection>2019-10-20/2019-10-24</ddm:datesOfCollection>
          <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
        </ddm:dcmiMetadata>
    )

    assume(schemaIsAvailable)
    validate(expectedDDM) shouldBe Success(())

    testConfig("archaeology").ddmTransformer.transform(ddmIn, "eas-dataset:123").map(normalized)
      .getOrElse(fail("no DDM returned")) shouldBe normalized(expectedDDM)
  }

  it should "not convert one-and-a-half dates pair" in {
    val ddmIn = ddm(title = "datesOfCollection  test", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
          <dc:date>2019-10-20 (startdatum)</dc:date>
          <dc:date>2019-10-24 (einddatum)</dc:date>
          <dc:date>2019-10-24 (einddatum)</dc:date>
        </ddm:dcmiMetadata>
    )
    val expectedDDM = ddm(title = "datesOfCollection test", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
          <dc:date>2019-10-20 (startdatum)</dc:date>
          <dc:date>2019-10-24 (einddatum)</dc:date>
          <dc:date>2019-10-24 (einddatum)</dc:date>
          <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
        </ddm:dcmiMetadata>
    )
    testConfig("archaeology").ddmTransformer.transform(ddmIn, "eas-dataset:123").map(normalized)
      .getOrElse(fail("no DDM returned")) shouldBe normalized(expectedDDM)
  }

  it should "convert a start only dates pair" in {
    val ddmIn = ddm(title = "datesOfCollection  test", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
          <dc:date>2019-10-20 (startdatum)</dc:date>
        </ddm:dcmiMetadata>
    )
    val expectedDDM = ddm(title = "datesOfCollection test", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
          <ddm:datesOfCollection>2019-10-20/</ddm:datesOfCollection>
          <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
        </ddm:dcmiMetadata>
    )
    testConfig("archaeology").ddmTransformer.transform(ddmIn, "eas-dataset:123").map(normalized)
      .getOrElse(fail("no DDM returned")) shouldBe normalized(expectedDDM)
  }

  it should "convert ad end only dates pair" in {
    val ddmIn = ddm(title = "datesOfCollection  test", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
          <dc:date>2019-10-20 (einddatum)</dc:date>
        </ddm:dcmiMetadata>
    )
    val expectedDDM = ddm(title = "datesOfCollection test", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
          <ddm:datesOfCollection>/2019-10-20</ddm:datesOfCollection>
          <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
        </ddm:dcmiMetadata>
    )
    testConfig("archaeology").ddmTransformer.transform(ddmIn, "eas-dataset:123").map(normalized)
      .getOrElse(fail("no DDM returned")) shouldBe normalized(expectedDDM)
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

    testConfig("archaeology").ddmTransformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe Success(normalized(ddmExpected))
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

    testConfig("archaeology").ddmTransformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
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
    testConfig("archaeology").ddmTransformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
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
                valueURI={ s"https://data.cultureelerfgoed.nl/term/id/abr/4e0dd439-480f-4039-bccc-4ce5ca2cbc05" }
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

    testConfig("archaeology").ddmTransformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
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
                valueURI={ s"https://data.cultureelerfgoed.nl/term/id/abr/e06a84fa-62e8-42ff-8f38-0ddfe9485a15" }
                subjectScheme="ABR verwervingswijzen"
              >Archeologisch bureauonderzoek en gecombineerd verkennend en karterend booronderzoek</ddm:acquisitionMethod>
              <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
        </ddm:dcmiMetadata>
    )

    testConfig("archaeology").ddmTransformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
      Success(normalized(expectedDdm))
  }

  it should "not replace the rightsHolder" in {
    val ddmIn = ddm(title = "Locatie 'Wadensteinse steeg' te Herwijnen, gemeente Lingewaal.", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
           <dct:alternative>Een bureauonderzoek</dct:alternative>
           <dct:rightsHolder>Jacobs en Burnier Projectbureau</dct:rightsHolder>
        </ddm:dcmiMetadata>
    )
    val expectedDdm = ddm(title = "Locatie 'Wadensteinse steeg' te Herwijnen, gemeente Lingewaal.", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
           <ddm:acquisitionMethod
              schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/554ca1ec-3ed8-42d3-ae4b-47bcb848b238"
              valueURI="https://data.cultureelerfgoed.nl/term/id/abr/d4ecc89b-d52e-49a1-880a-296db5c2953e"
              subjectScheme="ABR verwervingswijzen"
           >Een bureauonderzoek</ddm:acquisitionMethod>
           <dct:rightsHolder>Jacobs en Burnier Projectbureau</dct:rightsHolder>
        </ddm:dcmiMetadata>
    )

    testConfig("archaeology").ddmTransformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
      Success(normalized(expectedDdm))
  }

  "ddmTransformer" should "add inCollection for archaeology" in {
    val profile = <dc:title>blabla</dc:title><dct:description/> +: creator +: created +: available +: archaeology +: openAccess
    val ddmIn = ddm(
      <ddm:profile>{ profile }</ddm:profile>
      <ddm:dcmiMetadata/>
    )
    val inOneCollection = Map("easy-dataset:123" -> Seq(<inCollection>mocked</inCollection>))
    val transformer = new DdmTransformer(cfgDir, "archaeology", inOneCollection)

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
    val inOneCollection = Map("easy-dataset:123" -> Seq(<inCollection>mocked</inCollection>))
    val transformer = new DdmTransformer(cfgDir, "archaeology", inOneCollection)

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
    val inTwoCollections = Map("easy-dataset:123" -> Seq(<inCollection>mocked1</inCollection>, <inCollection>mocked2</inCollection>))
    val transformer = new DdmTransformer(cfgDir, "archaeology", inTwoCollections)

    transformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe Success(normalized(
      ddm(
        <ddm:profile>{ profile }</ddm:profile>
        <ddm:dcmiMetadata>
          <dc:title>asterix en obelix</dc:title>
          <dct:alternative>blabla rabarbera ratjetoe</dct:alternative>
          <inCollection>mocked1</inCollection>
          <inCollection>mocked2</inCollection>
          <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
        </ddm:dcmiMetadata>
      )))
  }
  it should "add rightsHolder with proper prefix" in {
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
    testConfig("archaeology").ddmTransformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
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
    testConfig("archaeology").ddmTransformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
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
    testConfig("archaeology").ddmTransformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
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

    testConfig("archaeology").ddmTransformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe Success(normalized(
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
    val archisIds = File("src/test/resources/possibleArchaeologyIdentifiers.txt")
      .lines(Charset.forName("UTF-8"))
      .filter(_.toLowerCase.contains("archis"))
      .map(id => <dct:identifier>{id}</dct:identifier>)
    val ddmIn = ddm(title = "blabla", audience = "D37000", dcmi =
          <ddm:dcmiMetadata>{ archisIds }</ddm:dcmiMetadata>
    )
    val triedNode = testConfig("archaeology").ddmTransformer.transform(ddmIn, "easy-dataset:123")
    val ddmOut = triedNode.getOrElse(fail("not expecting a conversion failure"))
    val strings = (ddmOut \\ "identifier").map(_.text)
    archisIds.size shouldNot be(strings.size)
    strings.filter(_.matches(".*[^0-9].*")) shouldBe Seq("10HZ-18 (Objectcode Archis)", "36141 (ARCHIS rapportnummer)", " 405800 (Archis nummers)", "http://livelink.archis.nl/Livelink/livelink.exe?func=ll&objId=4835986&objAction=browse (URI)", "66510 (Archisnummer)", "ARCHIS2: 63389", "Onderzoeksnaam Archis: 4042 Den Haag", "Objectnummer Archis: 1121031", "Archis2 nummer 65495", "3736 (RAAP) (Archis art. 41)", "6663 (ADC) (Archis art. 41)", "2866 (RAAP) (Archis art. 41)", "7104 (ADC) (Archis art. 41)", "16065 (BeVdG) (Archis art. 41)", "Archis2: CIS-code: 25499 (Tjeppenboer) en 25500 (Hilaard)")
  }
  it should "add dans license" in {
    val ddmIn = ddm(
      <ddm:profile><ddm:accessRights>REQUEST_PERMISSION</ddm:accessRights></ddm:profile>
          <ddm:dcmiMetadata/>
    )
    testConfig("archaeology").ddmTransformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe Success(normalized(ddm(
      <ddm:profile><ddm:accessRights>REQUEST_PERMISSION</ddm:accessRights></ddm:profile>
          <ddm:dcmiMetadata>
            <dcterms:license xsi:type="dcterms:URI">https://doi.org/10.17026/fp39-0x58</dcterms:license>
            <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
          </ddm:dcmiMetadata>
    )))
  }
  it should "replace funder role" in {
    val ddmIn = XML.loadFile("src/test/resources/funder/ddm-in.xml")
    val expectedDDM = XML.loadFile("src/test/resources/funder/ddm-out.xml")
    val triedDdm = testConfig("archaeology").ddmTransformer.transform(ddmIn, "easy-dataset:123")
    triedDdm.map(normalized) shouldBe Success(normalized(expectedDDM))
    assume(schemaIsAvailable)
    validate(ddmIn) should matchPattern {
      case Failure(e) if e.getMessage.contains("Funder") =>
    }
    validate(expectedDDM) shouldBe a[Success[_]]
  }
  it should "reject <AA><..><..>" in {
    (testDir / "ddm-encoding.xml").writeText(
      printer.format(Utility.trim(
        ddm(title = "Title <AA><80><93> of the <E2><80><98>dataset<e2><80><99>", audience = "D37000", dcmi = <ddm:dcmiMetadata/>)
      )).replaceAll("&lt;", "<").replaceAll("&gt;", ">")
    )
    val triedDdmIn = loadXml(testDir / "ddm-encoding.xml")
    triedDdmIn should matchPattern { case Failure(e: InvalidBagException)
      if e.getMessage.endsWith("The content of elements must consist of well-formed character data or markup.") =>
    }
  }
  it should "keep the original license for archaeology" in {
    val ddmIn = ddm(
      <ddm:profile><ddm:accessRights>OPEN_ACCESS_FOR_REGISTERED_USERS</ddm:accessRights></ddm:profile>
          <ddm:dcmiMetadata><dcterms:license xsi:type="dcterms:URI">http://does.not.exist.dans.knaw.nl</dcterms:license></ddm:dcmiMetadata>
    )
    testConfig("archaeology").ddmTransformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe Success(normalized(ddm(
      <ddm:profile><ddm:accessRights>OPEN_ACCESS_FOR_REGISTERED_USERS</ddm:accessRights></ddm:profile>
          <ddm:dcmiMetadata>
            <dcterms:license xsi:type="dcterms:URI">http://does.not.exist.dans.knaw.nl</dcterms:license>
            <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
          </ddm:dcmiMetadata>
    )))
  }

  it should "keep the original license for SSH" in {
    val ddmIn = ddm(
      <ddm:profile><ddm:accessRights>OPEN_ACCESS_FOR_REGISTERED_USERS</ddm:accessRights></ddm:profile>
          <ddm:dcmiMetadata><dcterms:license xsi:type="dcterms:URI">http://does.not.exist.dans.knaw.nl</dcterms:license></ddm:dcmiMetadata>
    )
    testConfig("SSH").ddmTransformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe Success(normalized(ddm(
      <ddm:profile><ddm:accessRights>OPEN_ACCESS_FOR_REGISTERED_USERS</ddm:accessRights></ddm:profile>
          <ddm:dcmiMetadata>
            <dcterms:license xsi:type="dcterms:URI">http://does.not.exist.dans.knaw.nl</dcterms:license>
            <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
          </ddm:dcmiMetadata>
    )))
  }

  "AmdTransformer" should "should not remove dates" in {
    val (ddmIn, _, _) = loadXml(File("src/test/resources/DD-857/dataset.xml"))
      .getOrElse(fail("could not load test data"))
    val (amdIn, _, _) = loadXml(File("src/test/resources/DD-857/amd.xml"))
      .getOrElse(fail("could not load test data"))


    // a few steps of EasyConvertBagToDepositApp.addProps
    val ddmOut = testConfig("SSH").ddmTransformer.transform(ddmIn, "easy-dataset:123")
      .getOrElse(fail("no DDM returned"))
    val amdOut = testConfig("SSH").amdTransformer.transform(amdIn, ddmOut \\ "created")
      .getOrElse(fail("no AMD returned"))

    normalized(amdIn) shouldBe normalized(amdOut)
  }

  "ZeroPosRewriteRule" should "drop '0 0' position" in {
    val ddmIn = ddm(title = "blabla", audience = "D37000", dcmi =
        <ddm:dcmiMetadata>
          <dcx-gml:spatial srsName="http://www.opengis.net/def/crs/EPSG/0/28992">
            <gml:Point>
              <gml:pos>0 0</gml:pos>
            </gml:Point>
          </dcx-gml:spatial>
          <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
        </ddm:dcmiMetadata>
    )

    testConfig("archaeology").ddmTransformer.transform(ddmIn, "easy-dataset:123").map(normalized) shouldBe
      Success(normalized(ddm(
        title = "blabla",
        audience = "D37000",
        dcmi = <ddm:dcmiMetadata>
                 <dcterms:rightsHolder>Unknown</dcterms:rightsHolder>
               </ddm:dcmiMetadata>,
      )))
  }
}
