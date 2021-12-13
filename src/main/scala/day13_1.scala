import scala.io.Source

object day13_1 extends App {

  type Matrix = List[List[Int]]

  sealed trait Fold {
    val value: Int
  }
  case class FoldX(value: Int) extends Fold
  case class FoldY(value: Int) extends Fold

  val filename  = "inputs/day13_1.txt"
  val source    = Source.fromFile(filename)
  val lines     = source.getLines().toList
  val separator = lines.indexWhere(_.isEmpty)
  val points = lines
    .slice(0, separator)
    .map(x => {
      val lst = x.split(",").map(_.toInt).toList
      (lst.head, lst.last)
    })

  val folds = lines
    .splitAt(separator + 1)
    ._2
    .map(x => {
      if (x.startsWith("fold along x="))
        FoldX(x.replaceAll("fold along x=", "").toInt)
      else
        FoldY(x.replaceAll("fold along y=", "").toInt)
    })

  val matrix: List[List[Int]] = (0 to points.map(_._2).max)
    .map(y =>
      (0 to points.map(_._1).max)
        .map(x => if (points.contains((x, y))) 1 else 0)
        .toList
    )
    .toList

  def fold(matrix: Matrix, fold: Fold) = fold match {
    case FoldX(v) => foldByX(matrix, v)
    case FoldY(v) => foldByY(matrix, v)
  }

  def foldByY(matrix: Matrix, value: Int): Matrix = {
    val parts   = matrix.splitAt(value)
    val partOne = parts._1
    val partTwo = parts._2.tail.reverse
    mergeMatrices(partOne, partTwo)
  }

  def foldByX(matrix: Matrix, value: Int) = {
    val transponsed = matrix.transpose
    val parts       = transponsed.splitAt(value)
    val partOne     = parts._1.transpose
    val partTwo     = parts._2.tail.reverse.transpose
    mergeMatrices(partOne, partTwo)
  }

  def mergeMatrices(m1: Matrix, m2: Matrix): Matrix =
    m1.indices
      .map(y =>
        m1.head.indices
          .map(x => {
            val p1 = m1(y)(x)
            val p2 = m2(y)(x)
            if (p1 == 1 || p2 == 1) 1 else 0
          })
          .toList
      )
      .toList

  val m1      = fold(matrix, folds.head)
  val result1 = m1.flatten.count(x => x == 1)
  println(s"result1 => $result1")

//  val m2 = fold(m1, folds.last)
//  printMatrix(m2)

  def printMatrix(m: Matrix): Unit = m.foreach(println)

}
