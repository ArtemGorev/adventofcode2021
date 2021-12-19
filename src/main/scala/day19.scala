import scala.io.Source

trait day19_fn {


  def readFile(path: String): Seq[String] = {
    val file = Source.fromFile(path)
    val seq = file.getLines().toSeq
    file.close()
    seq
  }

  def rotate(s: Scan, rotate: Rotation): Scan = {
    val rotated = rotate match {
      case Rotation(0, 0, 0, _) => s
      case Rotation(1, 0, 0, _) => s.copy(x = -s.x)
      case Rotation(0, 1, 0, _) => s.copy(y = -s.y)
      case Rotation(0, 0, 1, _) => s.copy(z = -s.z)
      case Rotation(1, 1, 0, _) => s.copy(x = -s.x, y = -s.y)
      case Rotation(0, 1, 1, _) => s.copy(y = -s.y, z = -s.z)
      case Rotation(1, 0, 1, _) => s.copy(x = -s.x, z = -s.z)
      case Rotation(1, 1, 1, _) => s.copy(x = -s.x, y = -s.y, z = -s.z)
    }

    val oriented = rotate match {
      case Rotation(_,_,_, Up) => s.copy()
      case Rotation(_,_,_, Down) => s.copy()
      case Rotation(_,_,_, Left) => s.copy()
      case Rotation(_,_,_, Right) => s.copy()
    }
  }

  def order(): List[Rotation] = {
    List[Rotation](
      Rotation(1, 0, 0),
      Rotation(0, 1, 0),
      Rotation(0, 0, 1),
      Rotation(1, 1, 0),
      Rotation(0, 1, 1),
      Rotation(1, 0, 1),
      Rotation(1, 1, 1)
    )
  }

  case class Scan(x: Int, y: Int, z: Int)
  case class Scanner(scans: Seq[Scan])
  case class Rotation(x: Int, y: Int, z: Int, orentation: Orientation)

  sealed class Orientation(degree: Int)
  object Up extends Orientation(0)
  object Down extends Orientation(180)
  object Left extends Orientation(270)
  object Right extends Orientation(90)
}

object day19 extends App with day19_fn {
  val filename = "inputs/day18_1.txt"


  val origin = Scan(-1, -1, 1)
  val scan = order().foldLeft[Scan](origin)((a: Scan, b: Rotation) => {
    val rotatedScan = rotate(a, b)
    println(s"rotatedScan => ${rotatedScan}")
    rotatedScan
  })

}
