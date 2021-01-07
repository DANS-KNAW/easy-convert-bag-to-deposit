package nl.knaw.dans.easy.bag2deposit

import better.files.File
import org.scalatest.flatspec.AnyFlatSpec

class TitlesSpec extends AnyFlatSpec {
  "" should "" in {
    val titles = File("src/test/resources/archeologischeTitels.txt")
      .lines.toSeq
      .map(_.split(",", 2))
      .map { case Array(id, titles) =>
        (
          id,
          titles
            .replaceAll(""""(.*)"""", "$1") // strip quotes
            .replaceAll("""\\,""", "%") // replace real commas
            .split(",")
            .map(_.replaceAll("%", ",")) // restore real commas
            .toSeq
        )
      }.toMap
    val rapporten = titles.values.toSeq
      .flatMap(_
        .map(_.toLowerCase.replaceAll(" [-_/a-z0-9.]+$", "")) // drop the supposed number at the tail
        .filter(str => !str.startsWith("thematische collectie: ")
          && str.matches(".*arch.*(rapport[a-z]+|notitie[a-z]+).*")
        )
      ).sortBy(identity)
      .distinct

    println(rapporten.mkString(s"${ rapporten.size }\n", "\n", ""))
  }
}
