import scala.annotation.tailrec
import scala.io.Source

object day6_1 extends App {

  val filename = "inputs/day6_1.txt"
  val source   = Source.fromFile(filename)
  val input    = source.getLines().toList.head.split(",").map(_.toInt).toList

  val partOneResult = calc(input, 80)
  println(s"part one result: ${partOneResult.size}")

  def calcOneDay(lst: List[Int]) = lst.flatMap(e => if (e == 0) List(6, 8) else List(e - 1))

  @tailrec
  def calc(lst: List[Int], days: Int): List[Int] = {
    if (days == 1)
      calcOneDay(lst)
    else
      calc(calcOneDay(lst), days - 1)
  }

}
