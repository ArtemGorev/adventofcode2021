import scala.annotation.tailrec
import scala.io.Source


object day15 extends App {

  type Level = Int
  type Coordinate = (Int, Int)
  type Matrix = Map[Coordinate, Level]
  type Path = Seq[(Int, Int)]

  val filename = "inputs/day15_1.txt"
  val source = Source.fromFile(filename)
  val lines = source.getLines().toList.filter(_.nonEmpty)
  val matrix = getMatrix(lines)

  println(s"part1 => ${path(matrix)}")
  println(s"part2 => ${path(expand(matrix))}")

  def path(grid: Matrix): Int = {
    val (start, end) = ((0, 0), grid.keys.maxBy(p => p._1 * p._2))


    @tailrec
    def dijkstra(todo: Set[Coordinate], risk: Matrix): Int = {

      val point = todo.minBy(risk)
      if (point == end) {
        risk(end)
      } else {
        val (nextTodo, nextRisk) = getNextSteps(point)
          .filter(grid.contains)
          .filter(next => !risk.contains(next) || risk(point) + grid(next) < risk(next))
          .foldLeft((todo - point, risk)) {
            case ((todo, risk), next) =>
              (todo + next, risk.updated(next, risk(point) + grid(next)))
          }
        dijkstra(nextTodo, nextRisk)
      }
    }

    dijkstra(Set(start), Map(start -> 0))
  }

  def getNextSteps(coordinate: Coordinate): Seq[Coordinate] =
    List((1, 0), (0, 1), (-1, 0), (0, -1))
      .map(add(coordinate, _))

  def expand(m: Matrix): Matrix = {
    val end = m.keys.maxBy(p => p._1 * p._2)
    val (width, height) = add(end, (1, 1))
    List.tabulate(5, 5) { (x, y) =>
      m.toSeq.map({ case (point: Coordinate, value: Int) =>
        (x * width + point._1, y * height + point._2) -> (1 + (value - 1 + x + y) % 9)
      })
    }.flatten.flatten.toMap
  }

  def add(a: Coordinate, b: Coordinate): Coordinate = (a._1 + b._1, a._2 + b._2)

  def getMatrix(lines: List[String]) = (
    for {
      y <- lines.indices
      x <- lines.head.indices
    } yield (x, y) -> lines(y)(x).toString.toInt
    ).toMap
}
