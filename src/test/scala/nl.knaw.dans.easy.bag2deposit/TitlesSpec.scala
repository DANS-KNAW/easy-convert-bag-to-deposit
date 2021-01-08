package nl.knaw.dans.easy.bag2deposit

import better.files.File
import org.scalatest.flatspec.AnyFlatSpec

class TitlesSpec extends AnyFlatSpec {
  private val titlesPerDataset = File("src/test/resources/archeologischeTitels.txt")
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
  private val nrRegExp = " [-_/a-z0-9]+\\.? *$"

  it should "show specific" in {
    val cfg = parseCsv(File("src/main/assembly/dist/cfg/ABR-reports.csv"), nrOfHeaderLines = 0)
      .map(r => ( r.get(1), s"${ r.get(2) }$nrRegExp"))
//    titlesPerDataset.foreach {
//      case (id, t) =>
//        t.map {title =>
//          cfg.filter { case (label,regexp) => title.matches(regexp)}
//        }
//    }
    cfg.foreach{case (label, regexp) =>
        titlesPerDataset
          .mapValues(_.filter(_.toLowerCase.matches(regexp)))
          .filter(_._2.nonEmpty)
          .foreach { case (id,t) => t.foreach(t => println(s"$label --- $id --- $t")) }
      }
  }
  it should "show general" in {

    // easy-dataset:103605,"...Zuidnederlandse Archeologische Rapporten 62..." heeft al een identifier: "ZAR 62"

    val mnemonics = "adc|baac|gra|mug|vestigia|zan|zar"
    val prefix = s"(ar(ch|g)a?eo[a-z]*|$mnemonics)"
    val suffix = "([a-z]*rapport|[a-z]*notitie|bericht)[a-z]*"
    val rapporten = titlesPerDataset.values.toSeq
      .flatMap(title => title
        .filter(_.matches(s".*$nrRegExp"))
        .map(_.toLowerCase.replaceAll(nrRegExp, "").replace(" [2017 - 116]", ""))
        .filter(_.matches(s"(.*$prefix[ -]$suffix.*|$mnemonics)"))
      ).sortBy(identity)
      .distinct

    println(rapporten.mkString(s"${ rapporten.size }\n", "\n", ""))
  }
  "" should "convert" in {
    // easy-dataset:41345,"Plangebied -¦t Hart
    // easy-dataset:19336,"Bureauonderzoek en inventariserend veldonderzoek Pingo-ruíne
    parseCsv(File("src/main/assembly/dist/cfg/ABR-reports.csv"), nrOfHeaderLines = 0)
      .foreach { r =>
        val regexp = r.get(1).toLowerCase
          // [^ -~\n‘’“”±áàäâçčćñčé̈èeêëïíóôöüûuúšžĐ¿»«Ø–‐¦¬\t„‟‘’“”´″·…] and 25 other odd characters
          .replaceAll("""[ "/(),-]+""","[^a-záàäâçčćèeêëïíóôöüûuú0-9]+")
          .replaceAll("archeo[a-z]*","ar(ch|g)a?eo[a-z]*")
          .replaceAll("rapport[a-z]*","rapport[a-z]*")
          .replaceAll("notitie[a-z]*","notitie[a-z]*")
          .replaceAll("publicaties?","publicaties?")
        println(s"""${ r.get(0) },"${ r.get(1) }","${ regexp }"""")
      }
  }
}
