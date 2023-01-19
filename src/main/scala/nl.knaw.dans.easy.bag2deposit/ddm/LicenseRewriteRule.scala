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
import nl.knaw.dans.easy.bag2deposit.parseCsv
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.csv.CSVFormat
import org.apache.commons.lang3.StringUtils
import org.apache.commons.lang3.StringUtils.isBlank

import java.net.URI
import scala.util.Try
import scala.xml.{ NamespaceBinding, Node }
import scala.xml.transform.RewriteRule

case class LicenseRewriteRule(cfgDir: File) extends RewriteRule with DebugEnhancedLogging {
  private val supportedLicenses: List[URI] = parseCsv(cfgDir / "supported-licenses.txt", 0).toSeq
    .map(r => new URI(r.get(0)))
    .toList


  private val variantToLicense: Map[String, String] = {
    val csvFormat = CSVFormat.RFC4180
      .withHeader("Variant", "Normalized")
      .withDelimiter(',')
      .withRecordSeparator('\n')
    parseCsv(cfgDir / "license-uri-variants.csv", nrOfHeaderLines = 1, csvFormat)
      .map(record => record.get("Variant") -> record.get("Normalized")).toMap
  }

  override def transform(node: Node): Seq[Node] = {
    if (! isLicenseUri(node))
      node
    else {
      val correctURI = getLicenseUri(supportedLicenses, variantToLicense, node)
      <dct:license xsi:type="dct:URI">{ correctURI.toString }</dct:license>
    }
  }

  private def isLicenseUri(node: Node): Boolean = {
    if (!("license" == node.label) || !("http://purl.org/dc/terms/" == node.namespace) || !hasXsiType(node, "URI")) {
      false
    } else {
      // validate it is a valid URI
      Try(new URI(node.text.trim)).isSuccess
    }
  }

  private def hasXsiType(node: Node, xsiType: String): Boolean = {
    val attributes = node.attributes
    if (attributes == null) return false
    val binding = NamespaceBinding("xsi", "http://www.w3.org/2001/XMLSchema-instance", node.scope)
    val maybeTypes = attributes.get("http://www.w3.org/2001/XMLSchema-instance", binding, "type")
    if ( maybeTypes.isEmpty ) return false
    val types = maybeTypes.get
    for(item <- types){
      val text = item.text
      return xsiType == text || text.endsWith(":" + xsiType)
    }
    false

  }
  private def getLicenseUri(supportedLicenses: List[URI], variantToLicense: Map[String, String], licenseNode: Node): URI = {
    val licenseText = Option(licenseNode)
      .map(_.text)
      .map(_.trim)
      .map(removeTrailingSlash)
      .map(s => variantToLicense.getOrElse(s,s))
      .getOrElse(throw new IllegalArgumentException("License node is null"))

    try {
      if (!isLicenseUri(licenseNode))
        throw new IllegalArgumentException("Not a valid license node")
      var licenseUri = new URI(licenseText)
      val licenseUriFinal = licenseUri
      val maybeLicenseUri = normalizeScheme(supportedLicenses, licenseUri)
      if(maybeLicenseUri.equals(Option.empty))
        new IllegalArgumentException(String.format("Unsupported license: %s", licenseUriFinal))
      licenseUri = maybeLicenseUri.get
      supportedLicenses.find(v => v == licenseUri).getOrElse(throw new IllegalArgumentException(String.format("Unsupported license: %s", licenseUri)))
    } catch {
      case e: Exception =>
        logger.error("Invalid license URI: {}", licenseText, e)
        throw new IllegalArgumentException("Not a valid license URI", e)
    }
  }

  private def removeTrailingSlash(s: String): String = {
    if (s.endsWith("/")) return s.substring(0, s.length - 1)
    s
  }

  private def normalizeScheme(supportedLicenses: List[URI], uri: URI): Option[URI] = {
    val schemes: Set[String] = Set("https", "http")
    var result = Option.empty[URI]
    for (license <- supportedLicenses) {
      if (StringUtils.equals(license.getHost, uri.getHost)
        && StringUtils.equals(license.getPath, uri.getPath)
        && license.getPort == uri.getPort
        && StringUtils.equals(license.getQuery, uri.getQuery)
        && schemes.contains(uri.getScheme))
        result = Option(license)
    }
    result
  }
}
