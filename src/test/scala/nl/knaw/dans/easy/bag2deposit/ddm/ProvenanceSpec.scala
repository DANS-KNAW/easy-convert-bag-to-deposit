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

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProvenanceSpec extends AnyFlatSpec with Matchers {
  "Provenance" should "show diff" in {
    val ddmIn =
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

    val ddmOut =
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
      </ddm>
    Provenance(ddmIn, ddmOut, "EasyConvertBagToDepositApp x.y.z") shouldBe Some(
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
  }
}
