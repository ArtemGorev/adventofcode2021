import scala.collection.mutable

trait day21_2_fn {

  private val diracRollCounts = {
    val ds = for {
      r1 <- 1 to 3
      r2 <- 1 to 3
      r3 <- 1 to 3
    } yield r1 + r2 + r3
    ds.toList.distinct.map(d => (d, ds.count(_ == d)))
  }

  def playDiracGame(players: (Player, Player)): (Long, Long) = {
    val memo = mutable.Map.empty[(Player, Player), (Long, Long)]

    def helper(p1: Player, p2: Player): (Long, Long) = {
      memo.getOrElseUpdate((p1, p2), {
        if (p1.score >= 21)
          (1L, 0L)
        else if (p2.score >= 21)
          (0L, 1L)
        else {
          val rollsIt: Iterator[(Long, Long)] = for {
            (roll, rollCount) <- diracRollCounts.iterator
            newP1 = p1.move(roll)
            (p2Cnt, newP1Cnt) = helper(p2, newP1) // swapped
          } yield (rollCount * newP1Cnt, rollCount * p2Cnt)
          rollsIt.reduce((a, b) => (a._1 + b._1, a._2 + b._2))
        }
      })
    }

    helper(players._1, players._2)
  }

  case class Player(state: Long, score: Long = 0) {
    val positions: Seq[Long] = 1l to 10l


    def move(pos: Long): Player = {
      val newPos = (state + pos - 1) % 10 + 1
      Player(newPos, score + newPos)
    }
  }

}

object day21_2 extends App with day21_2_fn {
  println(playDiracGame((Player(2), Player(5))))
}

