import day8_1.source

import scala.io.Source

object day9_1 extends App {

  val filename = "inputs/day9_1.txt"
  val source   = Source.fromFile(filename)
  val lines = source
    .getLines()
    .toList
    .map(_.split("").toList.map(_.toInt))

  var result = 0

  for (r <- lines.indices) {
    for (c <- lines.head.indices) {
      val e: Int              = lines(r)(c)
      val adjacent: List[Int] = getAdjacent(lines, r, c)

      if (adjacent.forall(x => e < x)) {
        result += e + 1
      }
    }
  }

  println(s"part one => $result")

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
