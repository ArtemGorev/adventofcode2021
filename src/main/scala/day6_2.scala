import scala.annotation.tailrec
import scala.io.Source

object day6_2 extends App {

  val filename = "inputs/day6_1.txt"
  val source   = Source.fromFile(filename)
  val input    = source.getLines().toList.head.split(",").map(_.toInt).toList

  val c1 = scala.collection.mutable.Map[(Int, Int), List[Int]]()
  val c2 = scala.collection.mutable.Map[Int, BigDecimal]()

  val days = 128
  (0 to 8).map(fish => cache(fish, days, (a, d) => calc(List(a), d)))

  val sum = input
    .map(fish => c1((fish, days)).map(kidFish => c2(kidFish)).sum)
    .sum
  println(s"$sum")

  def cache(fish: Int, days: Int, fn: (Int, Int) => List[Int]) = {
    val population = fn(fish, days)
    c1 += ((fish, days) -> population)
    c2 += (fish         -> BigDecimal(population.length))
    population
  }

  def calcOneDay(lst: List[Int]) = lst.flatMap(e => if (e == 0) List(6, 8) else List(e - 1))

  @tailrec
  def calc(lst: List[Int], days: Int): List[Int] = {
    if (days == 1)
      calcOneDay(lst)
    else
      calc(calcOneDay(lst), days - 1)
  }

}
