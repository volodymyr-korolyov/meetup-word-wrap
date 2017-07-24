package org.fun.prog

object WordWrap extends App {

  def wrap(text: String, width: Int): List[String] = {
    wrap(text.split(' ').toList, width, Nil).reverse
  }

  def wrap(words: List[String], width: Int, wrappedLines: List[String]): List[String] = {
    (words, wrappedLines) match {
      case (Nil, _) => wrappedLines
      case (word :: wordsTail, Nil) => wrap(wordsTail, width, word :: Nil)
      case (word :: wordsTail, line :: lineTail) if fit(line, word, width) => wrap(wordsTail, width, line + " " + word :: lineTail)
      case (word :: wordsTail, line :: lineTail) if word.length > width =>
        val wordParts = split(word, width)
        wrap(wordParts._2 :: wordsTail, width, wordParts._1 :: line :: lineTail)
      case (word :: wordsTail, line :: lineTail) => wrap(wordsTail, width, word :: line :: lineTail)
    }
  }

  def fit(line: String, word: String, width: Int): Boolean = line.length + word.length < width

  def split(word: String, width: Int): (String, String) = (word.substring(0, width - 1) + "-", word.substring(width - 1))

  def format(text: String, width: Int): Unit = {
    println(text)
    println("---")
    wrap(text, width).foreach(println)
    println
  }

  format("aaa bb cc ddddd", 6)
  format("a verylongsingle word", 6)
}
