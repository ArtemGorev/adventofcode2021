import scala.annotation.tailrec

object day18 extends App {

  sealed trait Entity
  case class Number(var value: Int)             extends Entity
  case class Pair(var x: Entity, var y: Entity) extends Entity
  object Pair {
    def apply(x: Int, y: Int): Pair = {
      Pair(Number(x), Number(y))
    }
  }

  // examples
  val l1 = Pair(Pair(Number(1), Number(2)), Number(3))                  // [[1,2],3]
  val l2 = Pair(Pair(Number(1), Number(9)), Pair(Number(8), Number(5))) // [[1,9],[8,5]]
  val l3 = Pair(Pair(Pair(Pair(Number(1), Number(2)), Pair(Number(3), Number(4))),
                     Pair(Pair(Number(5), Number(6)), Pair(Number(7), Number(8)))),
                Number(9)) // [[[[1,2],[3,4]],[[5,6],[7,8]]],9]

  def isNumbersOnly(e: Entity) = e match {
    case Pair(Number(_), Number(_)) => true
    case _                          => false
  }

  // [[[[[9,8],1],2],3],4] => [[[[0,9],2],3],4]
  val example1 = Pair(Pair(Pair(Pair(Pair(9, 8), Number(1)), Number(2)), Number(3)), Number(4))

  // [[[[0,7],4],[7,[[8,4],9]]],[1,1]] => [[[[0,7],4],[15,[0,13]]],[1,1]]
  val example2 = Pair(Pair(Pair(Pair(0, 7), Number(4)), Pair(Number(7), Pair(Pair(8, 4), Number(9)))), Pair(1, 1))

  // [[6,[5,[4,[3,2]]]],1]
  val example3 = Pair(Pair(Number(6), Pair(Number(5), Pair(Number(4), Pair(3, 2)))), Number(1))

  val e1 = explode(example2)
  val e2 = split(e1)

  println(e1)
  println(e2)
  println(split(e2))

  case class State(var leftTarget: Option[Pair], var rightTarget: Option[Pair])

  def explode(e: Entity) = {

    val state = State(None, None)

    def walk(e: Entity, step: Int = 1): Unit = {
      e match {
        case p @ Pair(Pair(_, _), Number(_)) => state.rightTarget = Some(p)
        case p @ Pair(Number(_), Pair(_, _)) => state.leftTarget = Some(p)
        case _                               => ()
      }

      e match {
        case Pair(Number(x), Number(y)) if step == 5 => {

          state.leftTarget.foreach {
            case p @ Pair(Number(_), c @ Pair(_, _)) =>
              if (e == c) p.y = Number(0)
              p.x.asInstanceOf[Number].value += x
            case _ => ()
          }

          state.rightTarget.foreach {
            case p @ Pair(c @ Pair(_, _), Number(_)) =>
              if (e == c) p.x = Number(0)
              p.y.asInstanceOf[Number].value += y
            case _ => ()
          }
        }

        case Pair(Number(_), Number(_)) => ()
        case Pair(l, Number(_))         => walk(l, step + 1)
        case Pair(Number(_), r)         => walk(r, step + 1)
        case Pair(l, r) =>
          walk(l, step + 1)
          walk(r, step + 1)
      }
    }

    walk(e)
    e
  }

  def split(e: Entity) = {

    def walk(e: Entity): Entity = e match {
      case Pair(Number(x), e) if x >= 10  => Pair(getPair(x), e)
      case Pair(e, Number(y)) if y >= 10  => Pair(e, getPair(y))
      case p @ Pair(Number(_), Number(_)) => p
      case Pair(l, n @ Number(_))         => Pair(walk(l), n)
      case Pair(n @ Number(_), r)         => Pair(n, walk(r))
      case Pair(l, r)                     => Pair(walk(l), walk(r))
    }

    walk(e)
  }

  def getPair(x: Int) = {
    val half = x / 2
    Pair(Number(half), Number(x - half))
  }

}
