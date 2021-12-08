import scala.io.Source

object day8_1 extends App {

  val filename = "inputs/day8_1.txt"
  val source   = Source.fromFile(filename)
  val lines = source
    .getLines()
    .toList
    .map(s => {
      val inputAndOutput = s.split(" \\| ")
      val input          = inputAndOutput(0).split(" ").toList
      val output         = inputAndOutput(1).split(" ").toList
      (input, output)
    })
  val line = lines.head

  val input = line._1.map(_.toCharArray)
  val map   = getMap(input)
  val part2 = lines
    .map(a => {
      val digits: Map[String, String] = getMap(a._1.map(_.toCharArray))
      a._2.map((number: String) => digits(number.sorted)).mkString.toInt
    })
    .sum
  val part1: Int = lines
    .map(_._2.count(a => shouldBeLen(a)))
    .sum

  println(s"part1 => $part1")
  println(s"part2 => $part2")

  def getMap(input: Seq[Array[Char]]): Map[String, String] = {
    val digit1 = input.find(_.length == 2).get
    val digit4 = input.find(_.length == 4).get
    val digit7 = input.find(_.length == 3).get
    val digit8 = input.find(_.length == 7).get

    val digit9 = input
      .filter(_.length == 6)
      .filter(contains(_, digit4))
      .filter(contains(_, digit7))
      .head
    val digit0: Array[Char] = input
      .filter(_.length == 6)
      .filter(contains(_, digit1))
      .filterNot(contains(_, digit9))
      .head
    val digit6 = input
      .filter(_.length == 6)
      .filterNot(contains(_, digit0))
      .filterNot(contains(_, digit9))
      .head
    val digit3 = input
      .filter(_.length == 5)
      .find(contains(_, digit1))
      .get

    val digit2 = input
      .filter(_.length == 5)
      .filter(contains(_, digit8.diff(digit9)))
      .head

    val digit5 = input
      .filter(_.length == 5)
      .filterNot(contains(_, digit2))
      .filterNot(contains(_, digit3))
      .head

    Map(
      digit0.sorted.mkString -> "0",
      digit1.sorted.mkString -> "1",
      digit2.sorted.mkString -> "2",
      digit3.sorted.mkString -> "3",
      digit4.sorted.mkString -> "4",
      digit5.sorted.mkString -> "5",
      digit6.sorted.mkString -> "6",
      digit7.sorted.mkString -> "7",
      digit8.sorted.mkString -> "8",
      digit9.sorted.mkString -> "9"
    )
  }

  def contains(parent: Array[Char], child: Array[Char]): Boolean =
    child.forall(p => parent.contains(p))

  private def shouldBeLen(a: String) =
    a.length == 2 ||
      a.length == 3 ||
      a.length == 4 ||
      a.length == 7
}
