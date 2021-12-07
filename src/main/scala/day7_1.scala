import scala.io.Source

object day7_1 extends App {

  val filename = "inputs/day7_1.txt"
  val source   = Source.fromFile(filename)
  val crabs    = source.getLines().toList.head.split(",").map(_.toInt).toList

  def findMinFuel(lst: List[Int]) =
    (lst.min to lst.max).map(x => lst.map(y => Math.abs(x - y)).sum).min

  def findMinFuel2(lst: List[Int]) =
    (lst.min to lst.max)
      .map(x => lst.map(y => sum(Math.abs(x - y))).sum)
      .min
      .toInt

  def sum(n: Int) = ((1 + n) / 2.0) * n

  println(s"part one result: ${findMinFuel(crabs)}")
  println(s"part two result: ${findMinFuel2(crabs)}")

}
