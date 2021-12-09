import day8_1.source

import scala.io.Source

object day9_1 extends App {

  val filename = "inputs/day9_1.txt"
  val source   = Source.fromFile(filename)
  val lines = source
    .getLines()
    .toList
    .map(_.split("").toList.map(_.toInt))

//  var result1 = 0
//
//  for (r <- lines.indices) {
//    for (c <- lines.head.indices) {
//      val e: Int              = lines(r)(c)
//      val adjacent: List[Int] = getAdjacent(lines, r, c)
//
//      if (adjacent.forall(x => e < x)) {
//        result1 += e + 1
//      }
//    }
//  }
//
//  println(s"part one => $result1")

  def walkThrough(m: List[List[Int]], row: Int, column: Int): List[(Int, Int)] = {
    val currentValue              = getValue(m, row, column)
    val nearest: List[(Int, Int)] = getNearestIndexes(row, column)
    val next: List[(Int, Int)] = nearest
      .filter {
        case (r, c) =>
          val value = getValue(m, r, c)
          value != 9 && value - currentValue == 1
      }

    if (next.isEmpty) {
      List((row, column))
    } else {
      next.flatMap(x => walkThrough(m, x._1, x._2)) :+ (row, column)
    }
  }

  var result2 = List[(Int, Int)]()

  for (r <- lines.indices) {
    for (c <- lines.head.indices) {
      val e: Int              = lines(r)(c)
      val adjacent: List[Int] = getAdjacent(lines, r, c)

      if (adjacent.forall(x => e < x)) {
        result2 = result2 :+ (r, c)
      }
    }
  }

  val q = result2
    .map(x => walkThrough(lines, x._1, x._2).distinct.map(x => getValue(lines, x._1, x._2)).length)
    .sorted
    .takeRight(3)
    .product

  println(q)

  def getValue(m: List[List[Int]], row: Int, column: Int): Int = {
    val rowsCount    = m.length - 1
    val columnsCount = m.head.length - 1

    if (row < 0 || row > rowsCount) {
      9
    } else if (column < 0 || column > columnsCount) {
      9
    } else {
      m(row)(column)
    }
  }

  def getNearestIndexes(row: Int, column: Int): List[(Int, Int)] = {
    val lst = List[(Int, Int)](
      (0, -1),
      (-1, 0),
      (0, 1),
      (1, 0)
    )

    lst.map(x => (x._1 + row, x._2 + column))
  }

  def getAdjacent(m: List[List[Int]], row: Int, column: Int): List[Int] = {
    val rowsCount    = m.length - 1
    val columnsCount = m.head.length - 1

    // up edge
    if (row == 0) {
      // up left corner
      if (column == 0) {
        List(
          m(row + 1)(column),
          m(row)(column + 1)
        )
        // up right corner
      } else if (column == columnsCount) {
        List(
          m(row + 1)(column),
          m(row)(column - 1)
        )
      } else {
        List(
          m(row + 1)(column),
          m(row)(column + 1),
          m(row)(column - 1)
        )
      }
      // bottom edge
    } else if (row == rowsCount) {
      if (column == 0) {
        List(
          m(row - 1)(column),
          m(row)(column + 1)
        )
        // up right corner
      } else if (column == columnsCount) {
        List(
          m(row - 1)(column),
          m(row)(column - 1)
        )
      } else {
        List(
          m(row - 1)(column),
          m(row)(column + 1),
          m(row)(column - 1)
        )
      }
      // left edge
    } else if (column == 0) {
      List(
        m(row - 1)(column),
        m(row + 1)(column),
        m(row)(column + 1)
      )
      // right edge
    } else if (column == columnsCount) {
      List(
        m(row - 1)(column),
        m(row + 1)(column),
        m(row)(column - 1)
      )
    } else {
      List(
        m(row - 1)(column),
        m(row + 1)(column),
        m(row)(column - 1),
        m(row)(column + 1)
      )
    }
  }

}
