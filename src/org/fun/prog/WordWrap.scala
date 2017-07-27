package org.fun.prog

//noinspection TypeAnnotation
object WordWrap extends App {
  type Line = List[String]

  def wrap(text: String, width: Int): String = {
    val lines = wrap(text.split(' ').toList, width, List(List()))
    lines
      .reverse
      .map(_.reverse.mkString(" "))
      .mkString("\n")
  }

  def wrap(words: List[String], width: Int, accLines: List[Line]): List[Line] = {
    (words, accLines) match {
      case (Nil, _) => accLines
      case (word :: wordsTail, line :: lineTail) => (word, line, width) match {

        case LongWordSplits(wordEnd, newLine) => wrap(wordEnd :: wordsTail, width, newLine :: accLines)

        case WordFitsLine(_, updatedLine) => wrap(wordsTail, width, updatedLine :: lineTail)

        case _ => wrap(wordsTail, width, List(word) :: accLines)
      }
    }
  }

  val LongWordSplits = Extractor({
    case (word, line, width) if word.length > width => Some((word.substring(width), List(word.substring(0, width))))
    case _ => None
  })

  val WordFitsLine = Extractor({
    case (word, line, width)  if lineLength(word :: line) <= width => Some("", word :: line)
    case _ => None
  })

  def lineLength( line: Line) = line.mkString(" ").length

  case class Extractor(f: ((String, Line, Int)) => Option[(String, Line)]) {
    def unapply(v: (String, Line, Int)):Option[(String, Line)] = f(v)
  }

  def format(text: String, width: Int): Unit = {
    println(text)
    println("---")
    println(wrap(text, width))
    println
  }

  format("aaa bb cc ddddd", 6)
  format("a verylongsingle word", 6)
  format("word word", 3)
  format("word word word", 9)
}
