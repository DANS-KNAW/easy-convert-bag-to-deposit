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
package nl.knaw.dans.easy.bag2deposit.Fixture

import scala.xml.{ Elem, NodeBuffer }

trait DdmSupport {

  val creator: Elem =     <dcx-dai:creatorDetails>
                            <dcx-dai:organization>
                              <dcx-dai:name>DANS</dcx-dai:name>
                            </dcx-dai:organization>
                          </dcx-dai:creatorDetails>
  val created: Elem =     <ddm:created>2013-03-01</ddm:created>
  val available: Elem =   <ddm:available>2013-04-01</ddm:available>
  val archaeology: Elem = <ddm:audience>D37000</ddm:audience>
  val openAccess: Elem =  <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
  val schemaLocations: String =
    """http://easy.dans.knaw.nl/schemas/md/ddm/ http://easy.dans.knaw.nl/schemas/md/2015/12/ddm.xsd
      |http://www.den.nl/standaard/166/Archeologisch-Basisregister/ http://easy.dans.knaw.nl/schemas/vocab/2012/10/abr-type.xsd
      |http://www.w3.org/2001/XMLSchema-instance http://easy.dans.knaw.nl/schemas/md/emd/2013/11/xml.xsd
      |http://purl.org/dc/terms/ http://easy.dans.knaw.nl/schemas/emd/2013/11/qdc.xsd
      |http://purl.org/dc/elements/1.1/ http://dublincore.org/schemas/xmls/qdc/dc.xsd
      |""".stripMargin

  def ddm(content: NodeBuffer): Elem =
      <ddm:DDM xmlns:dcx="http://easy.dans.knaw.nl/schemas/dcx/"
         xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xmlns:abr="http://www.den.nl/standaard/166/Archeologisch-Basisregister/"
         xmlns:dc="http://purl.org/dc/elements/1.1/"
         xmlns:dct="http://purl.org/dc/terms/"
         xmlns:dcterms="http://purl.org/dc/terms/"
         xmlns:dcx-dai="http://easy.dans.knaw.nl/schemas/dcx/dai/"
         xmlns:dcmitype="http://purl.org/dc/dcmitype/"
         xsi:schemaLocation={ schemaLocations }
      >{ content }</ddm:DDM>

  def ddm(title: String, audience: String, dcmi: Elem): Elem =
      <ddm:DDM xmlns:dcx="http://easy.dans.knaw.nl/schemas/dcx/"
         xmlns:ddm="http://easy.dans.knaw.nl/schemas/md/ddm/"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xmlns:abr="http://www.den.nl/standaard/166/Archeologisch-Basisregister/"
         xmlns:dc="http://purl.org/dc/elements/1.1/"
         xmlns:dct="http://purl.org/dc/terms/"
         xmlns:dcterms="http://purl.org/dc/terms/"
         xmlns:dcx-dai="http://easy.dans.knaw.nl/schemas/dcx/dai/"
         xmlns:dcmitype="http://purl.org/dc/dcmitype/"
         xsi:schemaLocation={ schemaLocations }
      >
        <ddm:profile>
          <dc:title>{ title }</dc:title><dct:description/>{ creator +: created +: available }
          <ddm:audience>{ audience }</ddm:audience>{ openAccess }
        </ddm:profile>
        { dcmi }
      </ddm:DDM>
}
