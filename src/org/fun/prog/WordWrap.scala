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
      case (word :: wordsTail, line :: lineTail) => WordContext(word, line, width) match {

        case WordSplits(start, end) => wrap(end :: wordsTail, width, start :: line :: lineTail)

        case WordFitsLine(updatedLine) => wrap(wordsTail, width, updatedLine :: lineTail)

        case _ => wrap(wordsTail, width, word :: line :: lineTail)
      }
    }
  }

  object WordSplits {
    def unapply(v: WordContext): Option[(String, String)] = v match {
      case _ if v.word.length > v.width => Some((v.word.substring(0, v.width - 1) + "-", v.word.substring(v.width - 1)))
      case _ => None
    }
  }

  object WordFitsLine {
    def unapply(v: WordContext): Option[String] = v match {
      case _ if v.word.length + v.line.length <= v.width => Some(v.line + " " + v.word)
      case _ => None
    }
  }

  case class WordContext(word: String, line: String, width: Int) {}

  def format(text: String, width: Int): Unit = {
    println(text)
    println("---")
    wrap(text, width).foreach(println)
    println
  }

  format("aaa bb cc ddddd", 6)
  format("a verylongsingle word", 6)
}
