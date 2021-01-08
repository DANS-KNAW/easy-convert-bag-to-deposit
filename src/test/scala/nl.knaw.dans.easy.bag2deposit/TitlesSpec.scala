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
            .split("""(,|\\n *)""")
            .map(_.replaceAll("%", ",")) // restore real commas
            .filter(_.trim.nonEmpty)
            .sortBy(identity)
            .distinct // e.g. easy-dataset:34135, easy-dataset:99840
            .toSeq
        )
      }.toMap

    // a special single one easy-dataset:99840 "Arcadis Archeologische Rapporten [2017 - 116]"
    val nrRegExp = " [-_/a-z0-9]+\\.? *$"

    // easy-dataset:103605,"...Zuidnederlandse Archeologische Rapporten 62..." heeft al een identifier: "ZAR 62"

    val mnemonics = "adc|baac|gra|mug|vestigia|zan|zar"
    val prefix = s"(ar(ch|g)a?eo[a-z]*|$mnemonics)"
    val suffix = "([a-z]*rapport|[a-z]*notitie|bericht)[a-z]*"
    val rapporten = titles.values.toSeq
      .flatMap(title => title
        .filter(_.matches(s".*$nrRegExp"))
        .map(_.toLowerCase.replaceAll(nrRegExp, "").replace(" [2017 - 116]", ""))
        .filter(_.matches(s"(.*$prefix[ -]$suffix.*|$mnemonics)"))
      ).sortBy(identity)
      .distinct

    println(rapporten.mkString(s"${ rapporten.size }\n", "\n", ""))
  }
}
