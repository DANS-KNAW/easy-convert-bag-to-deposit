package nl.knaw.dans.easy.bag2deposit.ddm

import better.files.File
import nl.knaw.dans.easy.bag2deposit.ddm
import nl.knaw.dans.easy.bag2deposit.ddm.ReportRewriteRule.nrRegexp

object CheckLists extends App {
  private val rule: ReportRewriteRule = ddm.ReportRewriteRule(File("src/main/assembly/dist/cfg"))
  private val testDir = File("target/checklists")

  def identifiersLists = {
    val identifiers = File("src/test/resources/possibleArchaeologyIdentifiers.txt")
      .lines.filter(_.matches(".*[ (].*"))
    val results = identifiers.flatMap(id =>
      rule.transform(<identifier>{ id }</identifier>)
    ).groupBy(_.label)
    (testDir / "identifiers-matched").writeText(
      results("reportNumber").map(_.text).mkString("\n")
    )
    val missed = results("identifier").groupBy(
      _.text.toLowerCase.matches(".*(notitie|rapport|bericht|publicat).*")
    )
    (testDir / "identifiers-missed-with-keyword").writeText(
      missed(true).map(_.text).mkString("\n")
    )
    (testDir / "identifiers-missed-otherwise").writeText(
      missed(false).map(_.text).mkString("\n")
    )
  }

  def titlesLists = {
    val titlesPerDataset = File("src/test/resources/archeologischeTitels.txt")
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
    lazy val titles = titlesPerDataset.values.flatten.toSeq
    val results = titles.flatMap(id =>
      rule.transform(<title>{ id }</title>)
    ).groupBy(_.label)
    (testDir / "titles-matched").writeText(
      results("reportNumber").map(_.text).mkString("\n")
    )
    val missed = results("title").groupBy(title => rule.reportMap.exists(cfg =>
      title.text.toLowerCase.matches(s".*${ cfg.regexp }[^a-z]+ $nrRegexp"))
    )
    (testDir / "titles-missed-at-end").writeText(
      missed(true).map(_.text).mkString("\n")
    )
    val missedInTheMiddle = missed(false).groupBy(
      _.text.toLowerCase.matches(".*(notitie|rapport|bericht|publicat).*")
    )
    (testDir / "titles-missed-with-keyword").writeText(
      missedInTheMiddle(true).map(_.text).mkString("\n")
    )
    (testDir / "titles-missed-otherwise").writeText(
      missedInTheMiddle(false).map(_.text).mkString("\n")
    )
  }

  if (testDir.exists) testDir.delete()
  testDir.createDirectories()
  identifiersLists
  titlesLists
}
