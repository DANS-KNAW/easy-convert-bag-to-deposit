package nl.knaw.dans.easy.bag2deposit

import better.files.File
import org.apache.commons.csv.CSVFormat.RFC4180

import java.nio.charset.Charset
import scala.util.Try
import scala.xml.{ Elem, Node }
import scala.xml.transform.{ RewriteRule, RuleTransformer }

class AgreementsTransformer(cfgDir: File) {
  private val userMap = parseCsv(
    cfgDir / "account-substitutes.csv",
    nrOfHeaderLines = 1,
    format = RFC4180.withHeader("old", "new"),
  ).map(r => r.get("old") -> r.get("new")).toMap

  private val userRewriteRule: RewriteRule = new RewriteRule {
    override def transform(node: Node): Seq[Node] = {
      if (!Seq("depositorId", "signerId").contains(node.label)) node
      else userMap
        .get(node.text).map(id => <id>{ id }</id>.copy(label = node.label))
        .getOrElse(node)
    }
  }
  private val transformer = new RuleTransformer(userRewriteRule)

  // The default charset is determined during virtual-machine startup and typically
  // depends upon the locale and charset of the underlying operating system.
  implicit val charset: Charset = Charset.forName("UTF-8")

  def transform(file: File): Try[(Elem, Node)] = {
    for {
      xmlIn <- loadXml(file)
      xmlOut = transformer.transform(xmlIn).headOption
        .getOrElse(throw new Exception("programming error: AgreementsTransformer returned multiple roots"))
      _ = file.writeText(xmlOut.serialize)
    } yield (xmlIn, xmlOut)
  }
}
