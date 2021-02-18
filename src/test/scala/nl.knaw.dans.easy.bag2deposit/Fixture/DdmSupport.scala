package nl.knaw.dans.easy.bag2deposit.Fixture

import scala.xml.Elem

trait DdmSupport {
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
         xsi:schemaLocation="
         http://easy.dans.knaw.nl/schemas/md/ddm/ http://easy.dans.knaw.nl/schemas/md/2015/12/ddm.xsd
         http://www.den.nl/standaard/166/Archeologisch-Basisregister/ http://easy.dans.knaw.nl/schemas/vocab/2012/10/abr-type.xsd
         http://www.w3.org/2001/XMLSchema-instance http://easy.dans.knaw.nl/schemas/md/emd/2013/11/xml.xsd
         http://purl.org/dc/terms/ http://easy.dans.knaw.nl/schemas/emd/2013/11/qdc.xsd
         http://purl.org/dc/elements/1.1/ http://dublincore.org/schemas/xmls/qdc/dc.xsd
      ">
        <ddm:profile>
          <dc:title>{ title }</dc:title>
          <dct:description>YYY</dct:description>
          <dcx-dai:creatorDetails>
            <dcx-dai:organization>
              <dcx-dai:name>DANS</dcx-dai:name>
            </dcx-dai:organization>
          </dcx-dai:creatorDetails>
          <ddm:created>2013-03</ddm:created>
          <ddm:available>2013-04</ddm:available>
          <ddm:audience>{ audience }</ddm:audience>
          <ddm:accessRights>OPEN_ACCESS</ddm:accessRights>
        </ddm:profile>
        { dcmi }
      </ddm:DDM>

}
