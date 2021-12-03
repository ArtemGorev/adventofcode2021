import java.lang.Integer.parseInt
import scala.io.Source

object day3_1 extends App {

  val filename = "inputs/day3_1.txt"
  val source   = Source.fromFile(filename)
  val lines    = source.getLines().toList
  val count: Array[Int] = lines
    .map(_.split("").map(_.toInt))
    .reduce((a, b) => sum(a, b))
  val limit = lines.length / 2

  private val booleans = count.map(a => a > limit)

  val gammaRate = parseInt(booleans
                             .map(a => if (a) "1" else "0")
                             .reduce((a, b) => a + b),
                           2)

  val epsilonRate = parseInt(booleans
                               .map(a => !a)
                               .map(a => if (a) "1" else "0")
                               .reduce((a, b) => a + b),
                             2)

  def sum(a: Array[Int], b: Array[Int]): Array[Int] = a.zip(b).map(a => a._1 + a._2)

  println(s"gammaRate => $gammaRate")
  println(s"epsilonRate => $epsilonRate")

  println(s"${gammaRate * epsilonRate}")
}
