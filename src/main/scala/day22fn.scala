import scala.annotation.tailrec
import scala.io.Source

trait day22fn {

  type Cuboids = Seq[Cuboid]
  type State = (Cuboids, RebootStep)

  def parse(filename: String): List[RebootStep] = {
    //    on x=10..12,y=10..12,z=10..12
    Source.fromFile(filename).getLines()
      .map(line => {
        val state = line.startsWith("on ")
        val numbers = line
          .replaceFirst(if (state) "on " else "off ", "")
          .replace("x=", "")
          .replace("y=", "")
          .replace("z=", "")
          .split(",").flatMap(pair => pair.split("\\.\\.")).map(_.toInt)

        RebootStep(state,
          Cuboid(
            Range(numbers(0), numbers(1)),
            Range(numbers(2), numbers(3)),
            Range(numbers(4), numbers(5))
          )
        )
      }).toList
  }

  def simulateReboot(steps: List[RebootStep]): Set[Point] =
    steps.foldLeft(Set.empty[Point])(simulateStep)

  def simulateStep(poss: Set[Point], step: RebootStep): Set[Point] =
    if (step.state)
      poss ++ step.cuboid.points
    else
      poss.filterNot(step.cuboid.contains)

  def countReboot(steps: Seq[RebootStep]): Long = {
    type Section = (RebootStep, Int)

    @tailrec
    def helper(sections: Seq[Section], sign: Int, acc: BigInt): Long = {
      if (sections.isEmpty)
        acc.toLong
      else {
        val onSectionsSize = sections.view
          .collect({
            case (RebootStep(true, box), _) => box.getValue()
          })
          .sum
        val newAcc = acc + sign * onSectionsSize
        val newSections = for {
          (RebootStep(sectionOn, sectionBox), sectionI) <- sections
          (RebootStep(stepOn, stepBox), stepI) <- steps.view.zipWithIndex.drop(sectionI + 1)
          if !(!sectionOn && stepOn)
          intersection <- sectionBox intersect stepBox
        } yield (RebootStep(sectionOn || stepOn, intersection), stepI)
        helper(newSections, -sign, newAcc)
      }
    }

    helper(steps.zipWithIndex, +1, 0L)
  }

  case class Cuboid(x: Range, y: Range, z: Range) {
    def intersect(that: Cuboid): Option[Cuboid] = {
      val start = min max that.min
      val end = max min that.max

      if (start <= end)
        Some(Cuboid(
          Range(start.x, end.x),
          Range(start.y, end.y),
          Range(start.z, end.z),
        ))
      else
        None
    }

    def min: Point = Point(x = x.start, y = y.start, z = z.start)

    def max: Point = Point(x = x.end, y = y.end, z = z.end)

    def -(that: Cuboid): Cuboid = {
      Cuboid(
        x overlap that.x,
        y overlap that.y,
        z overlap that.z,
      )
    }

    def points: Seq[Point] =
      for {
        _x <- x.iterator()
        _y <- y.iterator()
        _z <- z.iterator()
      } yield Point(_x, _y, _z)

    def isEmpty: Boolean =
      x.isEmpty || y.isEmpty || z.isEmpty

    def contains(that: Cuboid): Boolean = this.x.contains(that.x) && this.y.contains(that.y) && this.z.contains(that.z)

    def contains(point: Point): Boolean = this.x.contains(point.x) && this.y.contains(point.y) && this.z.contains(point.z)

    def getValue(): Long = x.size * y.size * z.size
  }

  case class RebootStep(state: Boolean, cuboid: Cuboid) {
    def getValue(): Long = cuboid.getValue()

    def apply(that: RebootStep): Seq[RebootStep] = {
      (this, that) match {
        case (RebootStep(true, a), RebootStep(true, b)) => List(this, RebootStep(state = false, a - b))
        case (RebootStep(false, a), RebootStep(false, b)) => List(this, RebootStep(state = true, a - b))
        case (_, _) => List(this)

      }
    }
  }

  case class Range(start: Long, end: Long) {
    def iterator(): Seq[Long] = (start to end).toList

    def size: Long = end - start + 1

    def isEmpty: Boolean = start > end

    def overlap(that: Range): Range = {
      if (this.contains(that.start) && this.contains(that.end)) {
        that
      } else if (this.contains(that.start)) {
        Range(that.start, this.end)
      } else if (this.contains(that.end)) {
        Range(this.start, that.end)
      } else {
        Range(1, -1)
      }
    }

    def contains(value: Long): Boolean = start <= value && value <= end

    def contains(that: Range): Boolean = this.contains(that.start) && this.contains(that.end)

  }

  case class Point(x: Long, y: Long, z: Long) {
    def max(that: Point) = Point(x max that.x, y max that.y, z max that.z)

    def min(that: Point) = Point(x min that.x, y min that.y, z min that.z)

    def <=(that: Point): Boolean =
      x <= that.x && y <= that.y && z <= that.z
  }

  def countInits(steps: List[RebootStep]) = {
    val initializationBox = Cuboid(
      Range(-50, 50),
      Range(-50, 50),
      Range(-50, 50),
    )
    val smallSteps = steps.filter(step => initializationBox.contains(step.cuboid))
    countReboot(smallSteps).toInt
  }

}

object day22 extends App with day22fn {
  val cuboids = parse("inputs/day22_1.txt")

  println(s"part1 => ${countInits(cuboids)}")
  println(s"part2 => ${countReboot(cuboids)}")
}

