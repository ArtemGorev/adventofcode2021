import java.lang.Integer.parseInt
import scala.io.Source

object day17 extends App {

  case class Point(x: Int, y: Int)
  case class Velocity(v: Int, t: Int, endless: Boolean)
  case class Input(x: Range, y: Range)

  // "target area: x=20..30, y=-10..-5"
//  val input =  Input(Range.inclusive(20, 30), Range.inclusive(-10, -5))
  // target area: x=153..199, y=-114..-75
  val input =  Input(Range.inclusive(153, 199), Range.inclusive(-114, -75))

  println(s"part1 => ${(1 until Math.abs(input.y.start)).sum}")
  println(s"part2 => ${calculateDistinct(input)}")

  def evalRangesInX(limit: Range) = {
    (1 to limit.end).flatMap(
      vx => {
        var t = 0;
        var x = 0;
        var set = List[Velocity]()
        do {
          x += (vx - t)
          if(limit.contains(x))
            set = set :+ Velocity(vx, t, vx - t <= 0)
          t += 1
        } while(x < limit.end && (vx - t) >= 0)
        set
      }
    ).toList
  }

  def evalRangesInY(limit: Range): List[Velocity] = {
    val minVy = Math.min(limit.end, limit.start)
    val maxVy = Math.abs(minVy) - 1
    println(s"maxVy => ${maxVy}")
    println(s"minVy => ${minVy}")
    println(s"limit.end => ${limit.start}")
    (minVy  to maxVy).flatMap(
      vy => {
        var t = 0;
        var y = 0;
        var set = List[Velocity]()
        do {
          y += (vy - t)
          if(limit.contains(y))
            set = set :+ Velocity(vy, t, endless = false)
          t += 1
        } while(y > limit.end)
        set
      }
    ).toList
  }

  def canCombine(vx: Velocity, vy: Velocity, limit: Input) = {
    var t = 0;
    var y = 0;
    var x = 0;
    var targetIsReached = false;
    var targetIsPassed = false;
    do {
      y += (vy.v - t)
      if(vx.v - t > 0)
        x += (vx.v - t)
      t += 1

      targetIsReached = limit.x.contains(x) && limit.y.contains(y)
      targetIsPassed = limit.x.end < x || limit.y.start > y
    } while(!targetIsPassed && !targetIsReached)

    targetIsReached
  }


    //(vx.t == vy.t) || (vx.endless && vy.t >= vx.t)

  def calculateDistinct(input: Input) = {
    val xVelocities = evalRangesInX(input.x).distinctBy(_.v)
    val yVelocities = evalRangesInY(input.y)
    println(s"xVelocities => ${xVelocities.length}")
    println(s"yVelocities => ${yVelocities.length}")


    println(s"yVelocities => ${yVelocities.map(_.v).mkString(",")}")
    (for {
      vx <- xVelocities
      vy <- yVelocities if canCombine(vx, vy, input)
    } yield (vx, vy)).distinct.length
  }


  val res = List((23,-10),
  (25,-9),
  (27,-5),
  (29,-6),
  (22,-6),
  (21,-7),
  (9,0),
  (27,-7),
  (24,-5),
  (25,-7),
  (26,-6),
  (25,-5),
  (6,8),
  (11,-2),
  (20,-5),
  (29,-10),
  (6,3),
  (28,-7),
  (8,0),
  (30,-6),
  (29,-8),
  (20,-10),
  (6,7),
  (6,4),
  (6,1),
  (14,-4),
  (21,-6),
  (26,-10),
  (7,-1),
  (7,7),
  (8,-1),
  (21,-9),
  (6,2),
  (20,-7),
  (30,-10),
  (14,-3),
  (20,-8),
  (13,-2),
  (7,3),
  (28,-8),
  (29,-9),
  (15,-3),
  (22,-5),
  (26,-8),
  (25,-8),
  (25,-6),
  (15,-4),
  (9,-2),
  (15,-2),
  (12,-2),
  (28,-9),
  (12,-3),
  (24,-6),
  (23,-7),
  (25,-10),
  (7,8),
  (11,-3),
  (26,-7),
  (7,1),
  (23,-9),
  (6,0),
  (22,-10),
  (27,-6),
  (8,1),
  (22,-8),
  (13,-4),
  (7,6),
  (28,-6),
  (11,-4),
  (12,-4),
  (26,-9),
  (7,4),
  (24,-10),
  (23,-8),
  (30,-8),
  (7,0),
  (9,-1),
  (10,-1),
  (26,-5),
  (22,-9),
  (6,5),
  (7,5),
  (23,-6),
  (28,-10),
  (10,-2),
  (11,-1),
  (20,-9),
  (14,-2),
  (29,-7),
  (13,-3),
  (23,-5),
  (24,-8),
  (27,-9),
  (30,-7),
  (28,-5),
  (21,-10),
  (7,9),
  (6,6),
  (21,-5),
  (27,-10),
  (7,2),
  (30,-9),
  (21,-8),
  (22,-7),
  (24,-9),
  (20,-6),
  (6,9),
  (29,-5),
  (8,-2),
  (27,-8),
  (30,-5),
  (24,-7)
  )
  println(s"x.uniq ${res.map(_._1).distinct.length}")
  println(s"y.uniq ${res.map(_._2).distinct.length}")
  println(s"y.uniq ${res.map(_._2).distinct.sorted.mkString(",")}")
}
