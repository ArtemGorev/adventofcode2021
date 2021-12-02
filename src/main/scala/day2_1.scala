import scala.io.Source

object day2_1 extends App {

  sealed trait Movement
  case class Forward(value: Int) extends Movement
  case class Down(value: Int)    extends Movement
  case class Up(value: Int)      extends Movement

  case class Position(horizontal: Int = 0, depth: Int = 0)

  val filename                    = "inputs/day2_1.txt"
  val source                      = Source.fromFile(filename)
  val listOfStrings: List[String] = source.getLines().toList
  val listOfMovements: List[Movement] = listOfStrings.map(s => {
    val arr = s.split(" ")
    arr(0) match {
      case "down" => Down(arr(1).toInt)
      case "up"   => Up(arr(1).toInt)
      case _      => Forward(arr(1).toInt)
    }
  })

  val resultPosition = listOfMovements.foldLeft(Position())((acc: Position, curr: Movement) =>
    curr match {
      case Forward(value) => acc.copy(horizontal = acc.horizontal + value)
      case Down(value)    => acc.copy(depth = acc.depth + value)
      case Up(value)      => acc.copy(depth = acc.depth - value)
    }
  )

  source.close()

  println(s"result => ${resultPosition.horizontal * resultPosition.depth}")
}
