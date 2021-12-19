import scala.annotation.tailrec
import scala.io.Source

trait day19_fn {


  def readFile(path: String): Seq[String] = {
    val file = Source.fromFile(path)
    val seq = file.getLines().toSeq
    file.close()
    seq
  }

  type Scanner = Set[Point]

  def parseScanners(lines: Seq[String]): Seq[Scanner] = lines.mkString("\n").split("\n\n").map(parseScanner)

  def parseScanner(s: String): Scanner = s.linesIterator
    .drop(1)
    .map(line => {
      val Seq(x, y, z) = line.split(",").toSeq
      Point(x.toInt, y.toInt, z.toInt)
    }).toSet

  def getAmountOfBeacons(scanners: Seq[Scanner]): Int = createMap(scanners)._1.size

  def createMap(scanners: Seq[Scanner]): (Scanner, Set[Point]) = {

    @tailrec
    def helper(scanners: Seq[Scanner], beacons: Scanner, next: Scanner, scannerPoints: Set[Point]): (Scanner, Set[Point]) = {
      val newBeacons = beacons ++ next
      if (scanners.isEmpty)
        (newBeacons, scannerPoints)
      else {
        val (matchedScanners, orientedScanners, matchedScannerPoss) = (for {
          scanner <- scanners
          (orientedScanner, scannerPos) <- matchScanner(next, scanner)
        } yield (scanner, orientedScanner, scannerPos)).unzip3
        val newScanners = scanners.filterNot(matchedScanners.contains)
        val nextScanner = orientedScanners.reduce(_ ++ _)
        val newScannerPoints = scannerPoints ++ matchedScannerPoss
        helper(newScanners, newBeacons, nextScanner, newScannerPoints)
      }
    }

    val scanner0 +: scannersTail = scanners
    helper(scannersTail, Set.empty, scanner0, Set(Point(0, 0, 0))) // start with scanner 0 fixed
  }

  def matchScanner(beacons: Scanner, scanner: Scanner): Option[(Scanner, Point)] =
    (for {
      orientedScanner <- scannerOrientations(scanner).toList
      ds1 = for {
        p1: Point <- beacons.toList
        p2: Point <- orientedScanner.toList
      } yield {
        p1 - p2
      }
      ds = groupCount(ds1)
      (d, cnt) <- ds if cnt >= 12
    } yield (orientedScanner.map(_ + d), d)).headOption

  def groupCount(ds: Seq[Point]) = ds.toList.distinct.map(d => (d, ds.count(_ == d)))

  def scannerOrientations(scanner: Scanner): Seq[Scanner] = scanner.toSeq.map(orientate).transpose.map(a => a.toSet)

  def orientate(point: Point): Seq[Point] = {
    val Point(x, y, z) = point
    Seq(
      Point(x, y, z),
      Point(x, -y, -z),
      Point(x, -z, y),
      Point(x, z, -y),
      Point(-x, z, y),
      Point(-x, -y, z),
      Point(-x, y, -z),
      Point(-x, -z, -y),
      Point(-y, x, z),
      Point(y, -x, z),
      Point(y, x, -z),
      Point(-y, -x, -z),
      Point(-z, y, x),
      Point(-z, x, -y),
      Point(-z, -y, -x),
      Point(-z, -x, y),
      Point(z, y, -x),
      Point(z, x, y),
      Point(z, -y, x),
      Point(z, -x, -y),
      Point(-y, -z, x),
      Point(y, -z, -x),
      Point(-y, z, -x),
      Point(y, z, x),
    )
  }

  def largestDistance(scanners: Seq[Scanner]): Int = {
    val (_, scannerPoss) = createMap(scanners)
    (for {
      p1 <- scannerPoss
      p2 <- scannerPoss
    } yield p1 manhattanDistance p2).max
  }

  sealed trait Orientation

  case class Point(x: Int, y: Int, z: Int) {
    def manhattanDistance(that: Point): Int =
      (x - that.x).abs + (y - that.y).abs + (z - that.z).abs

    def -(that: Point): Point =
      Point(x - that.x, y - that.y, z - that.z)

    def +(that: Point): Point =
      Point(x + that.x, y + that.y, z + that.z)
  }
}

object day19 extends App with day19_fn {
  val filename = "inputs/day19_1.txt"
  val scanners = parseScanners(readFile(filename))

  println(getAmountOfBeacons(scanners))
  println(largestDistance(scanners))
}
