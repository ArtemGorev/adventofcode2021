import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object day18 extends App {

  val filename = "inputs/day18_0.txt"
  val entities = Source.fromFile(filename).getLines().map(x => addParents(syn(lex(x)), None)).toList

  sealed trait Entity
  case class Pair(var x: Entity, var y: Entity, parent: Option[Entity] = None) extends Entity

  object Pair {
    def apply(x: Int, y: Int): Pair = {
      Pair(Number(x), Number(y))
    }
  }

  def syn(lexems: Seq[Lexem]): Entity = {
    val brackets       = mutable.Stack[Lexem]()
    val entities       = mutable.Stack[Entity]()
    var result: Entity = null

    for (elem <- lexems) {
      elem match {
        case LeftBracket => {
          brackets.push(elem)
        }
        case RightBracket => {
          val right = entities.pop()
          val left  = entities.pop()
          entities.push(Pair(left, right))
        }
        case Digit(number) =>
          entities.push(Number(number))
      }
    }

    entities.pop()
  }

  def lex(s: String): Seq[Lexem] = {
    var tmp = ""
    var res = List[Lexem]();

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

  case class Number(var value: Int, parent: Option[Entity] = None) extends Entity
  case class Digit(number: Int)                                    extends Lexem
  case object LeftBracket                                          extends Lexem
  case object RightBracket                                         extends Lexem

  val test1: Entity = syn(lex("[[1,2],3]"))
  val test2: Entity = syn(lex("[[1,9],[8,5]]"))
  val test3: Entity = syn(lex("[[[[1,2],[3,4]],[[5,6],[7,8]]],9]"))

  def isNumbersOnly(e: Entity) = e match {
    case Pair(Number(_, _), Number(_, _), _) => true
    case _                                   => false
  }

  // [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]
//  val example = addParents(syn(lex("[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]")), None)
//  explode(example)
//  println(printEntity(example))

  def addParents(e: Entity, parent: Option[Entity]): Entity = e match {
    case n @ Number(_, _) => n.copy(parent = parent)
    case Pair(l, r, _) => {
      val p = Pair(null, null, parent)
      p.x = addParents(l, Some(p))
      p.y = addParents(r, Some(p))
      p
    }
  }

  def process(e1: Entity, e2: Entity) = {
    println(printEntity(e1))
    println(printEntity(e2))
    var result: Entity = Pair(e1, e2)
    var before         = ""
    var after          = ""
    var before1        = ""

    do {
      before1 = printEntity(result)
      do {
        before = printEntity(result)
        explode(result)
        after = printEntity(result)
      } while (before != after)

      do {
        before = printEntity(result)
        result = split(result)
        after = printEntity(result)
      } while (before != after)

    } while (before1 != after)

    explode(result)
    result
  }

  println(printEntity(process(entities(0), entities(1))))

//  val e = addParents(syn(lex("[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]")), None)
//  explode(e)
//
//  println(printEntity(e))

  case class State(var leftTarget: Option[Pair], var rightTarget: Option[Pair])

  @tailrec
  def addLeftValue(pair: Pair, value: Int): Unit = {
    pair match {
      case Pair(n @ Number(_, _), _, _)  => n.value += value
      case Pair(p @ Pair(_, _, _), _, _) => addLeftValue(p, value)
    }
  }

  @tailrec
  def addRightValue(pair: Pair, value: Int): Unit = {
    pair match {
      case Pair(_, n @ Number(_, _), _)  => n.value += value
      case Pair(_, p @ Pair(_, _, _), _) => addRightValue(p, value)
    }
  }

  @tailrec
  def changeRight(start: Entity, e: Entity, value: Int): Unit =
    e match {
      case Pair(_, n @ Number(_, _), _)                           => n.value += value
      case Pair(_, p @ Pair(_, _, _), _) if p != start            => addLeftValue(p, value)
      case Pair(_, p @ Pair(_, _, _), Some(parent)) if p == start => changeRight(e, parent, value)
      case _                                                      => ()
    }

  @tailrec
  def changeLeft(start: Entity, e: Entity, value: Int): Unit =
    e match {
      case Pair(n @ Number(_, _), _, _)                           => n.value += value
      case Pair(p @ Pair(_, _, _), _, _) if p != start            => addLeftValue(p, value)
      case Pair(p @ Pair(_, _, _), _, Some(parent)) if p == start => changeLeft(e, parent, value)
      case _                                                      => ()
    }

  def explode(e: Entity) = {
    def walk(e: Entity, step: Int = 1): Unit = {
      e match {
        case Pair(Number(x, _), Number(y, _), Some(parent)) if step == 5 =>
          changeLeft(e, parent, x)
          changeRight(e, parent, y)
          parent match {
            case p @ Pair(x, _, _) if x == e => p.x = Number(0)
            case p @ Pair(_, y, _) if y == e => p.y = Number(0)
          }
        case Pair(Number(_, _), Number(_, _), _) => ()
        case Pair(l, Number(_, _), _)            => walk(l, step + 1)
        case Pair(Number(_, _), r, _)            => walk(r, step + 1)
        case Pair(l, r, _) =>
          walk(l, step + 1)
          walk(r, step + 1)
      }
    }

    walk(e)
    e
  }

  def printEntity(e: Entity): String = e match {
    case Pair(x, y, _) => s"[${printEntity(x)},${printEntity(y)}]"
    case Number(x, _)  => s"$x"
  }

  def split(e: Entity) = {

    def walk(e: Entity): Entity = e match {
      case Pair(Number(x, _), e, _) if x >= 10     => Pair(getPair(x), e)
      case Pair(e, Number(y, _), _) if y >= 10     => Pair(e, getPair(y))
      case p @ Pair(Number(_, _), Number(_, _), _) => p
      case Pair(l, n @ Number(_, _), _)            => Pair(walk(l), n)
      case Pair(n @ Number(_, _), r, _)            => Pair(n, walk(r))
      case Pair(l, r, _)                           => Pair(walk(l), walk(r))
    }

    walk(e)
  }

  def getPair(x: Int) = {
    val half = x / 2
    Pair(Number(half), Number(x - half))
  }

}
