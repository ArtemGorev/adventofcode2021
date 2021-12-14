import scala.io.Source

object day12 extends App {
  type Node  = String
  type Path  = String
  type Edge  = (Node, Node)
  type Rules = Seq[Edge]

  val rules: Rules = Source
    .fromFile("inputs/day12_0.txt")
    .getLines()
    .toList
    .map(line => {
      val strings = line.split("-")
      (strings(0), strings(1))
    })

  val allRules = (rules ++ rules.map(_.swap))
    .filterNot(node => node._2 == "start")
    .filterNot(node => node._1 == "end")

  val r = walkThrough(allRules, "start", List("start")).filter(x => x.contains("end"))

  println(r.length)

  def isBigCave(node: Node) = node.toUpperCase() == node

  def visits(result: Seq[Node], node: Node) = result.count(_ == node)

  def hasDoubleVisitedSmallCaves(result: Seq[Node]) = {
    val smalls = result.filter(node => node.toLowerCase() == node)
    (smalls.length - smalls.distinct.length) == 2
  }

  def nodeCanBeVisited(result: Seq[Node], node: Node) = {
    if (node == "start") {
      true
    } else {
      val amount  = visits(result, node)
      val bigCave = isBigCave(node)
      val doubleVisitedSmallCave =
        hasDoubleVisitedSmallCaves(result)

      bigCave || (amount <= 2 && !doubleVisitedSmallCave)
    }
  }

  def walkThrough(paths: Rules, start: Node, result: Seq[Node]): Seq[Seq[Node]] = {
    val endPoints = paths.filter(p => p._1 == start)

    if (start == "end") {
      List(result)
    } else if (nodeCanBeVisited(result, start)) {
      val value: Seq[Seq[Node]] = endPoints.flatMap(end => walkThrough(paths, end._2, result :+ end._2))

      value
    } else {
      List(result)
    }
  }

}
