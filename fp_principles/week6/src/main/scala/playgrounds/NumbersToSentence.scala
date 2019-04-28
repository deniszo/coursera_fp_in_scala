package playgrounds

import scala.io.Source

object NumbersToSentence {
  val fs = getClass.getResourceAsStream("/forcomp/linuxwords.txt")
  val words = Source.fromInputStream(fs).getLines.toList filter (_ forall(_.isLetter))

  val mnem = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  val charCode: Map[Char, Char] =
    for ((digit, str) <- mnem; ltr <- str) yield ltr -> digit


  def wordCode(word: String): String =
    word.toUpperCase map charCode

  val wordsForNum: Map[String, Seq[String]] =
    words groupBy wordCode withDefaultValue Seq()

  def encode(number: String): Set[List[String]] =
    if (number.isEmpty) Set(List())
    else {
      for {
        split <- 1 to number.length
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word :: rest
    }.toSet

  def translate(number: String): Set[String] =
    encode(number) map (_ mkString " ")

  def main(args: Array[String]): Unit = {
    println("charCodes", charCode)

    // 5282
    println("wordCode(Java)", wordCode("Java"))

    println("encode", encode("7225247386"))
    println("translate", translate("7225247386"))
  }
}
