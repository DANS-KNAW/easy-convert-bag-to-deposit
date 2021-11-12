package nl.knaw.dans.easy.bag2deposit.ddm

import scala.xml.transform.RewriteRule
import scala.xml.{ Elem, Node, Text }

object DateCreatedRewriteRule extends RewriteRule {
  override def transform(node: Node): Seq[Node] = node match {
    case e: Elem if e.label == "created" =>
      if (e.text.trim.length == 7)
        e.copy(child = Text(e.text.trim + "-01"))
      else if (e.text.trim.length == 4)
             e.copy(child = Text(e.text.trim + "-01-01"))
           else e
    case other => other
  }
}
