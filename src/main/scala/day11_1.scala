import day9_1.getValue

import scala.annotation.tailrec
import scala.io.Source

object day11_1 extends App {
  type Point  = (Int, Int)
  type Matrix = List[List[Int]]
  val filename = "inputs/day11_0.txt"
  val source   = Source.fromFile(filename)

  val matrix = source
    .getLines()
    .toList
    .map(_.split("").map(s => s.toInt).toList)

  printMatrix(matrix)
  var state: Matrix = matrix
  val DAYS          = 20000
  val part1 = (for {
    i <- 1 to DAYS if state.flatten.sum != 0
  } yield {
    state = doDay(state)
    println(s"-----DAY $i----")
    state.flatten.count(x => x == 0)
  }).sum

  print(s"part1 => $part1")

  def doDay(list: Matrix): Matrix = {

    @tailrec
    def rec(next: Matrix): Matrix = {
      val flashing = for {
        y <- next.indices
        x <- next.head.indices if next(y)(x) > 9
      } yield (x, y)

      if (flashing.isEmpty)
        next
      else {
        val nextFlashed = flashing.flatMap(f => getNearestIndexes(f))

        val next1 = for {
          y <- next.indices
          x <- next.head.indices
          count     = nextFlashed.count(p => p._1 == x && p._2 == y)
          value     = getValue(next, y, x)
          nextValue = value + count
        } yield {
          if (value > 9 || (value == 0))
            0
          else
            nextValue
        }
        rec(next1.toList.grouped(list.length).toList)
      }
    }

    val next = list.map(row => row.map(octopus => octopus + 1))
    rec(next)
  }

  def getAdjacent(m: Matrix, row: Int, column: Int): List[Int] =
    getNearestIndexes(row, column).map(p => getValue(m, p._1, p._2))

  def getNearestIndexes(point: Point): List[Point] = {
    val lst = List[Point](
      (0, -1),
      (-1, 0),
      (0, 1),
      (1, 0),
      (-1, -1),
      (-1, 1),
      (1, -1),
      (1, 1)
    )

    lst.map(x => concat(x, point))
  }

  def concat(a: Point, b: Point): Point = (a._1 + b._1, a._2 + b._2)

  def printMatrix(points: Matrix) = {
    for {
      y <- points.indices
      x <- points.head.indices
    } yield {
      print(points(y)(x))
      if (x == points.length - 1) println("")
    }
  }
}
