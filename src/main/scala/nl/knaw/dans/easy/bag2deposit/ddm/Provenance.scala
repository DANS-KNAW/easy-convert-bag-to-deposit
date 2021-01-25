package nl.knaw.dans.easy.bag2deposit.ddm

import nl.knaw.dans.easy.bag2deposit.{ Configuration, normalized }

import scala.xml.Node

object Provenance {
  def apply(generated: Node, modified: Node, version: String): Option[String] = {
    val original = normalized(generated).split("\n")
    val changed = normalized(modified).split("\n")
    val diff1 = original.diff(changed).mkString("\n").trim
    val diff2 = changed.diff(original).mkString("\n").trim
    if (diff1.nonEmpty || diff2.nonEmpty)
      Some(
        s"""===== only in old DDM
           |
           |$diff1
           |
           |===== only in new DDM by $version
           |
           |$diff2
           |""".stripMargin)
    else None
  }
}
