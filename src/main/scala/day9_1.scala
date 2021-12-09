import scala.io.Source

object day9_1 extends App {

  val filename = "inputs/day9_1.txt"
  val source   = Source.fromFile(filename)
  val lines = source
    .getLines()
    .toList
    .map(_.split("").toList.map(_.toInt))
  val minimums = getMinimums(lines)

  val part1 = getMinimums(lines).map(x => getValue(lines, x._1, x._2) + 1).sum
  val part2 = getMinimums(lines)
    .map(x => getBasin(lines, x._1, x._2))
    .filter(_.nonEmpty)
    .map(_.length)
    .sorted
    .takeRight(3)
    .product

  println(s"part1 => $part1")
  println(s"part2 => $part2")
  println(s"===============================================================")

  def getMinimums(m: List[List[Int]]) =
    for {
      x <- m.indices
      y <- m.head.indices
      if getAdjacent(m, x, y).forall(n => getValue(m, x, y) < n)
    } yield (x, y)

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

  type Point = (Int, Int)

  def getNearestIndexes(row: Int, column: Int): List[Point] = {
    val lst = List[Point](
      (0, -1),
      (-1, 0),
      (0, 1),
      (1, 0)
    )

    lst.map(x => (x._1 + row, x._2 + column))
  }

  def getAdjacent(m: List[List[Int]], row: Int, column: Int): List[Int] =
    getNearestIndexes(row, column).map(p => getValue(m, p._1, p._2))

  def getBasin(m: List[List[Int]], row: Int, column: Int): List[Point] = {
    val currentValue = getValue(m, row, column)
    val nearest      = getNearestIndexes(row, column)

    val next: List[Point] = nearest
      .filter { case (r, c) => getValue(m, r, c) != 9 }
      .filter { case (r, c) => getValue(m, r, c) > currentValue }

    if (next.isEmpty) {
      List((row, column))
    } else {
      next.flatMap(x => getBasin(m, x._1, x._2)).distinct :+ (row, column)
    }
  }
}
