package org.fun.prog

//noinspection TypeAnnotation
object WordWrap extends App {

  def wrap(text: String, width: Int): List[String] = {
    wrap(text.split(' ').toList, width, Nil).reverse
  }

  def wrap(words: List[String], width: Int, accLines: List[String]): List[String] = {
    (words, accLines) match {
      case (Nil, _) => accLines
      case (word :: wordsTail, Nil) => wrap(wordsTail, width, word :: Nil)
      case (word :: wordsTail, line :: lineTail) => (word, line, width) match {

        case LongWordSplits(wordEnd, newLine) => wrap(wordEnd :: wordsTail, width, newLine :: line :: lineTail)

        case WordFitsLine(_, updatedLine) => wrap(wordsTail, width, updatedLine :: lineTail)

        case _ => wrap(wordsTail, width, word :: line :: lineTail)
      }
    }
  }

  val LongWordSplits = Extractor({
    case (word, line, width) if word.length > width => Some((word.substring(width - 1), word.substring(0, width - 1) + "-"))
    case _ => None
  })

  val WordFitsLine = Extractor({
    case (word, line, width)  if word.length + line.length <= width => Some("", line + " " + word)
    case _ => None
  })

  case class Extractor(f: ((String, String, Int)) => Option[(String, String)]) {
    def unapply(v: (String, String, Int)):Option[(String, String)] = f(v)
  }

  def format(text: String, width: Int): Unit = {
    println(text)
    println("---")
    wrap(text, width).foreach(println)
    println
  }

  format("aaa bb cc ddddd", 6)
  format("a verylongsingle word", 6)
}
