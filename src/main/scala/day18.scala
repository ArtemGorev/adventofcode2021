import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

trait day18_fn {
  def parse(x: String): Entity = syn(lex(x))

  def syn(lexems: Seq[Lexem]): Entity = {
    val brackets = mutable.Stack[Lexem]()
    val entities = mutable.Stack[Entity]()

    for (elem <- lexems) {
      elem match {
        case LeftBracket =>
          brackets.push(elem)
        case RightBracket =>
          val right = entities.pop()
          val left = entities.pop()
          entities.push(Pair(left, right))
        case Digit(number) =>
          entities.push(Number(number))
      }
    }

    entities.pop()
  }

  def explode(entity: Entity, depth: Int): Option[(Option[Int], Entity, Option[Int])] = entity match {
    case Number(_) =>
      None
    case Pair(Number(left), Number(right)) if depth >= 5 =>
      Some((Some(left), Number(0), Some(right)))
    case Pair(left, right) =>
      explode(left, depth + 1).map { case (leftAdd, left, rightAdd) =>
        (leftAdd, Pair(left, rightAdd.map(right.addLeft).getOrElse(right)), None)
      } orElse explode(right, depth + 1).map { case (leftAdd, right, rightAdd) =>
        (None, Pair(leftAdd.map(left.addRight).getOrElse(left), right), rightAdd)
      }
  }


  def lex(s: String): Seq[Lexem] = {
    var tmp = ""
    var res = List[Lexem]()

    for (i <- 0 until s.length) {
      val currentSymbol = s(i)
      if (currentSymbol.isDigit) {
        tmp += currentSymbol
      } else {
        if (tmp.nonEmpty) {
          res = res :+ Digit(tmp.toInt)
          tmp = ""
        }

        currentSymbol match {
          case '[' => res = res :+ LeftBracket
          case ']' => res = res :+ RightBracket
          case ',' => res = res
        }
      }
    }

    res
  }

  //  [[1,2],3]
  sealed trait Lexem

  def explode(number: Entity): Option[Entity] = explode(number, 1).map(_._2)

  def split(number: Entity): Option[Entity] = number match {
    case Number(value) if value >= 10 =>
      val halfValue = value / 2
      Some(Pair(Number(halfValue), Number(value - halfValue)))
    case Number(_) =>
      None
    case Pair(left, right) =>
      split(left).map(Pair(_, right)) orElse split(right).map(Pair(left, _))
  }

  def present(e: Entity): String = e match {
    case Pair(x, y) => s"[${present(x)},${present(y)}]"
    case Number(x) => s"$x"
  }

  def addEntitiesMagnitude(entities: Seq[Entity]): Int = addEntities(entities).magnitude

  def addEntities(entities: Seq[Entity]): Entity = entities.reduce(_ + _)

  def largestTwoMagnitude(entities: Seq[Entity]): Int = {
    (for {
      left <- entities.iterator
      right <- entities.iterator
    } yield (left + right).magnitude).max
  }

  @tailrec
  private def reduce(entity: Entity): Entity = {
    explode(entity) match {
      case Some(entity) => reduce(entity)
      case None =>
        split(entity) match {
          case Some(entity) => reduce(entity)
          case None => entity
        }
    }
  }

  sealed trait Entity {
    def +(that: Entity): Entity = reduce(Pair(this, that))

    def addLeft(addValue: Int): Entity

    def addRight(addValue: Int): Entity

    def magnitude: Int
  }

  case class Pair(left: Entity, right: Entity) extends Entity {
    override def addLeft(addValue: Int): Entity = Pair(left.addLeft(addValue), right)

    override def addRight(addValue: Int): Entity = Pair(left, right.addRight(addValue))

    override def magnitude: Int = 3 * left.magnitude + 2 * right.magnitude
  }

  case class Number(var value: Int) extends Entity {
    override def addLeft(addValue: Int): Number = Number(value + addValue)

    override def addRight(addValue: Int): Number = Number(value + addValue)

    override def magnitude: Int = value
  }

  case class Digit(number: Int) extends Lexem

  case object LeftBracket extends Lexem

  case object RightBracket extends Lexem


}

object day18 extends App with day18_fn {

  val filename = "inputs/day18_1.txt"
  val entities = Source.fromFile(filename).getLines().map(parse).toSeq

  println(s"part1 => ${addEntitiesMagnitude(entities)}")
  println(s"part2 => ${largestTwoMagnitude(entities)}")
}
