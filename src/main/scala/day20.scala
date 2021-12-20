import scala.io.Source

trait day20_fn {
  def readFile(path: String): Seq[String] = {
    val file = Source.fromFile(path)
    val seq = file.getLines().toSeq
    file.close()
    seq
  }

  def parseMatrix(value: Seq[String]): Map[Point, Char] = (for {
    x <- value.indices
    y <- value(x).split("").indices
  } yield Point(x, y) -> value(y)(x)).toMap

  def process(input: Map[Point, Char], algo: String, char: Char): Map[Point, Char] =
    run(extend(input), algo, input, char)

  def extend(input: Map[Point, Char]): Map[Point, Char] = {
    val minX = input.keys.minBy(f => f.x).x - 1
    val maxX = input.keys.maxBy(f => f.x).x + 1
    val minY = input.keys.minBy(f => f.y).y - 1
    val maxY = input.keys.maxBy(f => f.y).y + 1

    List(
      (minX to maxX).map(x => Point(x, minY) -> '.'),
      (minX to maxX).map(x => Point(x, maxY) -> '.'),
      (minY to maxY).map(y => Point(minX, y) -> '.'),
      (minY to maxY).map(y => Point(maxX, y) -> '.'),
    ).flatten.toMap ++ input
  }

  def run(input: Map[Point, Char], algo: String, map: Map[Point, Char], char: Char): Map[Point, Char] =
    for {
      point <- input
    } yield point._1 -> calc(point._1, algo, map, char)

  def calc(point: Point, algo: String, input: Map[Point, Char], char: Char): Char = {
    val binaryString = getAdjacent(point)
      .map(p => input.getOrElse(p, char))
      .map {
        case '.' => "0"
        case '#' => "1"
      }.mkString("")
    algo(Integer.parseInt(binaryString, 2))
  }

  def getAdjacent(point: Point): Seq[Point] =
    List(
      (-1, -1), (0, -1), (1, -1),
      (-1, 0), (0, 0), (1, 0),
      (-1, 1), (0, 1), (1, 1),
    )
      .map(p => Point(p._1, p._2))
      .map(p => p + point)

  case class Point(x: Int, y: Int) {
    def +(that: Point): Point = Point(x + that.x, y + that.y)
  }

  def printMatrix(input: Map[Point, Char]): Unit = {
    val min = input.keys.minBy(p => p.x + p.y)
    val max = input.keys.maxBy(p => p.x + p.y)

    for {
      y <- min.y to max.y
      x <- min.x to max.x
    } yield {
      print(input(Point(x, y)))
      if (x == max.x) println("")
    }

  }
}

object day20 extends App with day20_fn {
  val filename = "inputs/day20_1.txt"
  val lines = readFile(filename)
  val algo = lines.head
  var input = parseMatrix(lines.drop(2))


  for (i <- 1 to 25) {
    val step1 = process(input, algo, '.')
    input = process(step1, algo, '#')

    if(i == 1)
      println(input.values.count(_ == '#'))
  }

  println(input.values.count(_ == '#'))
}
