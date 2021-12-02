import day2_1.{filename, resultPosition}

import scala.io.Source

object day2_2 extends App {

  sealed trait Movement
  case class Forward(value: Int) extends Movement
  case class Down(value: Int)    extends Movement
  case class Up(value: Int)      extends Movement

  case class Position(horizontal: Int = 0, depth: Int = 0, aim: Int = 0)

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

  val resultPosition = listOfMovements.foldLeft(Position())((pos: Position, mov: Movement) =>
    mov match {
      case Forward(value) => pos.copy(horizontal = pos.horizontal + value, depth = pos.depth + (pos.aim * value))
      case Down(value)    => pos.copy(aim = pos.aim + value)
      case Up(value)      => pos.copy(aim = pos.aim - value)
    }
  )

  source.close()

  println(s"result => ${resultPosition.horizontal * resultPosition.depth}")
}
