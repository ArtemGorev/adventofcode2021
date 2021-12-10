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

  val result1 = lines
    .map(findFirstIllegalChar)
    .filter(_.nonEmpty)
    .groupBy(x => x)
    .map(x => points(x._1) * x._2.length)
    .sum

  println(s"part one result => $result1")

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

  val result2 = lines
    .filter(l => findFirstIllegalChar(l).isEmpty)
    .map(completeString)
    .map(calcTotalScore)
    .sorted

  println(s"part two result => ${result2(result2.length / 2)}")

  def completeString(line: List[String]) = {
    val stack = mutable.Stack[String]()
    var chars = line

    while (chars.nonEmpty) {
      val char = chars.head
      chars = chars.tail
      if (openBrackets.contains(char)) {
        stack.push(char)
      } else {
        stack.pop()
      }
    }

    var result       = List[String]()
    val inversePairs = pairs.map(x => x.swap)

    while (stack.nonEmpty) {
      val char = stack.pop()
      result = result :+ inversePairs(char)
    }

    result
  }

  def calcTotalScore(lst: List[String]): Long = {
    val points = Map(
      ")" -> 1,
      "]" -> 2,
      "}" -> 3,
      ">" -> 4
    )

    lst.foldLeft(0.toLong)((acc, curr) => {
      val point = points(curr)
      acc * 5 + point
    })
  }
}
