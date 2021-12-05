import scala.collection.immutable.Nil
import scala.io.Source

object day5_1 extends App {

  type Ticket = List[List[Int]]
  val filename  = "inputs/day5_1.txt"
  val source    = Source.fromFile(filename)
  val rows      = source.getLines().toList
  val lineRegex = "(\\d+),(\\d+) -> (\\d+),(\\d+)".r
  val lines     = parseLines(rows)
  val hvpoints  = lines.map(produceHVPoints).reduce((a, b) => a.concat(b))
  val nonUnique = getOverlaped(hvpoints)
  println(s"first result => $nonUnique")
  val dpoints    = lines.map(produceDPoints).reduce((a, b) => a.concat(b))
  val allpoints  = dpoints.concat(hvpoints)
  val nonUnique2 = getOverlaped(allpoints)

  def getOverlaped(points: List[Point]) = points.diff(points.distinct).distinct.length

  println(s"second result => $nonUnique2")
  printMatrix(allpoints)

  def printMatrix(points: List[Point]) = {
    val maxX = points.map(p => p.x).max
    val maxY = points.map(p => p.y).max

    for {
      y <- 0 to maxX
      x <- 0 to maxY
      p = points.count(p => p.x == x & p.y == y)
    } yield {
      print(if (p == 0) "." else p)
      if (x == maxX) println("")
    }
  }

  def parseLines(rows: List[String]) =
    rows.map(parseLineR)

  def parseLineR(str: String): Line =
    str match {
      case lineRegex(a1, a2, b1, b2) =>
        Line(Point(a1.toInt, a2.toInt), Point(b1.toInt, b2.toInt))
    }

  def produceHVPoints(line: Line): List[Point] =
    if (line.start.x == line.end.x) {
      val start = Math.min(line.end.y, line.start.y)
      val end   = Math.max(line.end.y, line.start.y)
      (start to end).map(Point(line.end.x, _)).toList
    } else if (line.start.y == line.end.y) {
      val start = Math.min(line.end.x, line.start.x)
      val end   = Math.max(line.end.x, line.start.x)
      (start to end).map(Point(_, line.end.y)).toList
    } else {
      Nil
    }

  def produceDPoints(line: Line): List[Point] =
    if (line.start.x != line.end.x && line.start.y != line.end.y) {
      println(s"dline => $line")
      val dx   = if (line.start.x > line.end.x) -1 else 1
      val dy   = if (line.start.y > line.end.y) -1 else 1
      val max  = Math.max(line.start.y, line.end.y)
      val min  = Math.min(line.start.y, line.end.y)
      val list = (0 to max - min).map(a => Point(line.start.x + a * dx, line.start.y + a * dy)).toList
      println(s"points => $list")
      list
    } else Nil

  case class Point(x: Int, y: Int)

  case class Line(start: Point, end: Point)

}
