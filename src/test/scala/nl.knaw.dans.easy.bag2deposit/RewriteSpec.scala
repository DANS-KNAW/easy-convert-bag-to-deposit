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
import nl.knaw.dans.easy.bag2deposit.Fixture.SchemaSupport
import nl.knaw.dans.easy.bag2deposit.ddm.AbrRewriteRule
import org.apache.commons.csv.CSVRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.UUID
import scala.util.{ Success, Try }
import scala.xml.NodeBuffer

class RewriteSpec extends AnyFlatSpec with SchemaSupport with Matchers {
  private val cfgDir: File = File("src/main/assembly/dist/cfg")
  private val cfg = Configuration(cfgDir.parent)
  override val schema = "https://raw.githubusercontent.com/DANS-KNAW/easy-schema/9c2759d913cde537a2b49cbc0300532da56898c7/lib/src/main/resources/md/ddm/ddm.xsd"

  private val mandatoryInProfile =
          <dct:description>YYY</dct:description>
          <dcx-dai:creatorDetails>
            <dcx-dai:organization>
              <dcx-dai:name>DANS</dcx-dai:name>
            </dcx-dai:organization>
          </dcx-dai:creatorDetails>
          <ddm:created>2013-03</ddm:created>
          <ddm:available>2013-04</ddm:available>
          <ddm:audience>D35400</ddm:audience>
          <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>

  "ABR-complex" should "be valid" in {
    val records = parseCsv(cfgDir / "ABR-complex.csv", AbrRewriteRule.nrOfHeaderLines)
    records.map(tryUuid).filter(_.isFailure) shouldBe empty
    getDuplicates(records) shouldBe empty
  }

  "ABR-period" should "be valid" in {
    val records = parseCsv(cfgDir / "ABR-period.csv", AbrRewriteRule.nrOfHeaderLines)
    getDuplicates(records) shouldBe empty
    records.map(tryUuid).filter(_.isFailure) shouldBe empty
  }

  private def tryUuid(r: CSVRecord) = Try(UUID.fromString(r.get(2)))

  private def getDuplicates(records: Iterable[CSVRecord]) = records.groupBy(_.get(0)).filter(_._2.size > 1)

  "transform" should "convert" in {
    val ddmIn = ddm(
        <ddm:profile>
          <dc:title>Rapport 123</dc:title>
          { mandatoryInProfile }
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
    )

    val expectedDDM = ddm(
        <ddm:profile>
          <dc:title>Rapport 123</dc:title>
          { mandatoryInProfile }
        </ddm:profile>
        <ddm:dcmiMetadata>
            <dc:title>blabla</dc:title>
            <ddm:reportNumber
              schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e"
              valueURI="https://data.cultureelerfgoed.nl/term/id/abr/fcff6035-9e90-450f-8b39-cf33447e6e9f"
              subjectScheme="ABR Rapporten"
              reportNo="456"
            >Rapport 456</ddm:reportNumber>
            <ddm:reportNumber
              schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e"
              valueURI="https://data.cultureelerfgoed.nl/term/id/abr/90f3092a-818e-4db2-8467-35b64262c5b3"
              subjectScheme="ABR Rapporten"
              reportNo="2859"
            >Transect-rapport 2859</ddm:reportNumber>
            <dc:title>Transect-rapport 2859: Een Archeologisch Bureauonderzoek. Ellecom, glasvezeltracé Eikenstraat, Gemeente Rheden (GD).</dc:title>
            <dc:title>rabarbera</dc:title>
            <dc:title>Archeologische Berichten Nijmegen – Briefrapport 21</dc:title>
            <ddm:temporal xml:lang="nl"
                          valueURI="https://data.cultureelerfgoed.nl/term/id/abr/330e7fe0-a1f7-43de-b448-d477898f6648"
                          subjectScheme="Archeologisch Basis Register"
                          schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/b6df7840-67bf-48bd-aa56-7ee39435d2ed"
            >Vroege Middeleeuwen A</ddm:temporal>
            <ddm:subject xml:lang="nl"
                         valueURI="https://data.cultureelerfgoed.nl/term/id/abr/6ae3ab19-49ca-44a7-8b65-3a3395014bb9"
                         subjectScheme="Archeologisch Basis Register"
                         schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/b6df7840-67bf-48bd-aa56-7ee39435d2ed"
            >veenwinning (inclusief zouthoudend veen t.b.v. zoutproductie)</ddm:subject>
            <ddm:subject xml:lang="nl"
                         valueURI="https://data.cultureelerfgoed.nl/term/id/abr/f182d72c-2d22-47ae-b799-26dea01e770c"
                         subjectScheme="Archeologisch Basis Register"
                         schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/b6df7840-67bf-48bd-aa56-7ee39435d2ed"
            >akker / tuin</ddm:subject>
            <ddm:reportNumber
              schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e"
              valueURI="https://data.cultureelerfgoed.nl/term/id/abr/fcff6035-9e90-450f-8b39-cf33447e6e9f"
              subjectScheme="ABR Rapporten"
              reportNo="123"
            >Rapport 123</ddm:reportNumber>
        </ddm:dcmiMetadata>
    )

    new EasyConvertBagToDepositApp(cfg.copy(version = "x.y.z")).formatDiff(ddmIn, expectedDDM) shouldBe Some(
      """===== only in old DDM
        |
        |<dc:title>Rapport 456</dc:title>
        |<dcterms:temporal xsi:type="abr:ABRperiode">VMEA</dcterms:temporal>
        |<dc:subject xsi:type="abr:ABRcomplex">EGVW</dc:subject>
        |<dcterms:subject xsi:type="abr:ABRcomplex">ELA</dcterms:subject>
        |
        |===== only in new DDM by EasyConvertBagToDepositApp x.y.z
        |
        |<ddm:reportNumber  schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e" valueURI="https://data.cultureelerfgoed.nl/term/id/abr/fcff6035-9e90-450f-8b39-cf33447e6e9f" subjectScheme="ABR Rapporten" reportNo="456">
        | Rapport 456
        |</ddm:reportNumber>
        |<ddm:reportNumber  schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e" valueURI="https://data.cultureelerfgoed.nl/term/id/abr/90f3092a-818e-4db2-8467-35b64262c5b3" subjectScheme="ABR Rapporten" reportNo="2859">
        | Transect-rapport 2859
        |</ddm:reportNumber>
        |<ddm:temporal  xml:lang="nl" valueURI="https://data.cultureelerfgoed.nl/term/id/abr/330e7fe0-a1f7-43de-b448-d477898f6648" subjectScheme="Archeologisch Basis Register" schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/b6df7840-67bf-48bd-aa56-7ee39435d2ed">
        | Vroege Middeleeuwen A
        |</ddm:temporal>
        |<ddm:subject  xml:lang="nl" valueURI="https://data.cultureelerfgoed.nl/term/id/abr/6ae3ab19-49ca-44a7-8b65-3a3395014bb9" subjectScheme="Archeologisch Basis Register" schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/b6df7840-67bf-48bd-aa56-7ee39435d2ed">
        | veenwinning (inclusief zouthoudend veen t.b.v. zoutproductie)
        |</ddm:subject>
        |<ddm:subject  xml:lang="nl" valueURI="https://data.cultureelerfgoed.nl/term/id/abr/f182d72c-2d22-47ae-b799-26dea01e770c" subjectScheme="Archeologisch Basis Register" schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/b6df7840-67bf-48bd-aa56-7ee39435d2ed">
        | akker / tuin
        |</ddm:subject>
        |<ddm:reportNumber  schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/7a99aaba-c1e7-49a4-9dd8-d295dbcc870e" valueURI="https://data.cultureelerfgoed.nl/term/id/abr/fcff6035-9e90-450f-8b39-cf33447e6e9f" subjectScheme="ABR Rapporten" reportNo="123">
        | Rapport 123
        |</ddm:reportNumber>
        |""".stripMargin
    )

    cfg.ddmTransformer.transform(ddmIn).headOption.map(normalized)
      .getOrElse(fail("no DDM returned")) shouldBe normalized(expectedDDM)

    assume(schemaIsAvailable)
    validate(expectedDDM) shouldBe Success(())
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
