import day9_1.source

import scala.collection.mutable
import scala.io.Source

object day10_1 extends App {

  val openBrackets  = List("(", "[", "{", "<")
  val closeBrackets = List(")", "]", "}", ">")

  val points = Map(
    ""  -> 0,
    ")" -> 3,
    "]" -> 57,
    "}" -> 1197,
    ">" -> 25137
  )

  val pairs = Map(
    ")" -> "(",
    "]" -> "[",
    "}" -> "{",
    ">" -> "<"
  )

  val filename = "inputs/day10_1.txt"
  val source   = Source.fromFile(filename)
  val lines = source
    .getLines()
    .toList
    .map(_.split("").toList)

  val result = lines
    .map(findFirstIllegalChar)
    .filter(_.nonEmpty)
    .groupBy(x => x)
    .map(x => points(x._1) * x._2.length)
    .sum

  println(s"part one result => $result")

  def findFirstIllegalChar(line: List[String]) = {
    val stack       = mutable.Stack[String]()
    var chars       = line
    var illegalChar = ""
    while (chars.nonEmpty && illegalChar.isEmpty) {
      val char = chars.head
      chars = chars.tail
      if (openBrackets.contains(char)) {
        stack.push(char)
      } else {
        val last = stack.pop()
        if (pairs(char) != last) {
          illegalChar = char
        }
      }
    }
    illegalChar
  }

}
