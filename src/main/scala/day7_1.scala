import scala.io.Source

object day7_1 extends App {

  val filename = "inputs/day7_1.txt"
  val source   = Source.fromFile(filename)
  val crabs    = source.getLines().toList.head.split(",").map(_.toInt).toList

  def findMinFuel(lst: List[Int]) = {
    val min = lst.min
    val max = lst.max
    (min to max).map(x => lst.map(y => Math.abs(x - y)).sum).min
  }

  println(s"part one result: ${findMinFuel(crabs)}")

}
