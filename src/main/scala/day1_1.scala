import scala.io.Source

object day1_1 extends App {
  val source                      = Source.fromFile(filename)
  val filename                    = "inputs/day1_1.txt"
  val listOfStrings: List[String] = source.getLines().toList
  val listOfInts: List[Int]       = listOfStrings.map(_.toInt)
  val count = {
    listOfInts
      .foldLeft((listOfInts.head, 0))((acc, value) => {
        acc match {
          case (prev, cnt) if value > prev  => (value, cnt + 1)
          case (prev, cnt) if value <= prev => (value, cnt)
        }
      })
      ._2

  }

  print(s"count => $count");
  source.close()

}
