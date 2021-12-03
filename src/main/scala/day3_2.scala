import java.lang.Integer.parseInt
import scala.annotation.tailrec
import scala.io.Source

object day3_2 extends App {

  val filename    = "inputs/day3_1.txt"
  val source      = Source.fromFile(filename)
  val lines       = source.getLines().toList
  val linesOfInts = lines.map(_.split("").map(_.toInt))

//  println(s"Oxygen => ${calcOxygen(linesOfInts)}")
//  println(s"CO2 => ${calcCO(linesOfInts)}")
  println(calcOxygen(linesOfInts) * calcCO(linesOfInts))

  @tailrec
  def calcOxygen(l: List[Array[Int]], i: Int = 0): Int = {
    val bits = calcBits(l)
    val next =
      if (bits(i) == 1 || bits(i) == -1)
        l.filter(x => x(i) == 1)
      else
        l.filter(x => x(i) == 0)

    if (next.length == 1)
      parseInt(next.head.map(_.toString).reduce((a, b) => a + b), 2)
    else
      calcOxygen(next, i + 1)
  }

  @tailrec
  def calcCO(l: List[Array[Int]], i: Int = 0): Int = {
    val bits = calcBits(l).map(x => if (x == -1) x else if (x == 1) 0 else 1)
    val next =
      if (bits(i) == 1)
        l.filter(x => x(i) == 1)
      else
        l.filter(x => x(i) == 0)

    if (next.length == 1)
      parseInt(next.head.map(_.toString).reduce((a, b) => a + b), 2)
    else
      calcCO(next, i + 1)
  }

  def calcBits(l: List[Array[Int]]) = {
    val sumLst = l.reduce((a, b) => sum(a, b))
    sumLst.map(a => if (l.length - a == a) -1 else if (l.length - a < a) 1 else 0)
  }

  def sum(a: Array[Int], b: Array[Int]): Array[Int] = a.zip(b).map(a => a._1 + a._2)
}
