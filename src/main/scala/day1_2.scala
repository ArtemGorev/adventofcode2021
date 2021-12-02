import scala.io.Source

object day1_2 extends App {
  val filename                    = "inputs/day1_1.txt"
  val source                      = Source.fromFile(filename)
  val listOfStrings: List[String] = source.getLines().toList
  val listOfInts: List[Int]       = listOfStrings.map(_.toInt)
  val listOfSums                  = listOfInts.sliding(3).map(_.sum).toList
  val count                       = getCountOfIncreases(listOfSums.head, listOfSums)

  source.close()
  print(s"count => $count");

  private def getCountOfIncreases(first: Int, ints: List[Int]) =
    ints
      .foldLeft((first, 0))((acc, value) => {
        acc match {
          case (prev, cnt) if value > prev  => (value, cnt + 1)
          case (prev, cnt) if value <= prev => (value, cnt)
        }
      })
      ._2

}
