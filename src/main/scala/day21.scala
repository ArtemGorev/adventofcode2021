import scala.io.Source

trait day21_fn {

  def readFile(path: String): Seq[String] = {
    val file = Source.fromFile(path)
    val seq = file.getLines().toSeq
    file.close()
    seq
  }

  def playGame(startingPositions: List[Int]): (Player, Player) = {

    var dice = Dice(0)
    var players: List[Player] = startingPositions.map(Player(_, 0,0))
    do {
      players = players.map(p => {
        val (nextDice, throws) = dice.nextRolls()
        dice = nextDice
        val player = p.add(throws, dice.rolls)
        println(s"Player rolls $throws and moves to space ${player.state} for a total score of ${player.score} on ${dice.rolls}")
        player
      })
    } while (!hasWinner(players))

    (players.head, players.tail.head)
  }

  def hasWinner(state: List[Player]): Boolean = state.exists(_.score >= 1000)

  case class Player(state: Int, score: Int, dice: Int) {
    val positions: Seq[Int] = 1 to 10

    def add(throws: Seq[Int], rolls: Int): Player = {
      val sum = throws.sum
      val movement = move(state+sum)
      Player(movement, score + movement, rolls)
    }

    def move(pos: Int): Int = {
      var newPos = pos

      while(newPos > 10) {
        newPos = newPos - 10
      }

      newPos
    }
  }

  case class Dice(index: Int, rolls: Int = 0) {
    val range: Seq[Int] = 1 to 100

    def nextRolls(): (Dice, Seq[Int]) = (
      Dice(overflowCheck(index + 3), rolls + 3),
      List(
        range(overflowCheck(index)),
        range(overflowCheck(index + 1)),
        range(overflowCheck(index + 2))
      )
    )

    private def overflowCheck(state: Int): Int = {
      var nextState = state
      while(nextState >= 100) nextState -= 100
      nextState
    }
  }
}

object day21 extends App with day21_fn {
  val filename = "inputs/day21_0.txt"
  val lines = readFile(filename)

//  val startingPositions = List(4, 8)
  val startingPositions = List(2, 5)


  val (winner, loser) = playGame(startingPositions)
  println(winner, loser)
}

