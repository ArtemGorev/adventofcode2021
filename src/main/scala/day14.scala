import scala.io.Source

object day14 extends App {

  type PolymerTemplate = String
  type State           = Map[String, BigInt]
  type Rules           = Map[String, String]

  val filename = "inputs/day14_1.txt"
  val source   = Source.fromFile(filename)
  val lines    = source.getLines().toList.filter(_.nonEmpty)

  val origin = lines.head
  val rules: Rules = lines.tail
    .map(a => {
      val splited = a.split("->")
      splited(0).trim -> splited(1).trim
    })
    .toMap

  val steps = 40
  var state = rules.map(a => (a._1, countOccurrences(origin, a._1)))

  for (i <- 1 to steps) {
    state = doStep(state, rules)
    if (i == 10)
      println(s"part1 => ${calcResult(state, origin, rules)}")
  }
  println(s"part2 => ${calcResult(state, origin, rules)}")

  def doStep(state: State, rules: Rules): State = {
    val value = state
      .map(a => {
        val size   = a._2
        val letter = rules(a._1)
        val first  = a._1(0) + letter
        val second = letter + a._1(1)
        List((second, size), (first, size))
      })
      .flatten

    val value1 = value
      .groupMap(_._1)((tuple: (String, BigInt)) => tuple._2)
      .map(a => (a._1, a._2.sum))

    value1
  }

  def calcResult(state: State, origin: String, rules: Rules) = {
    val r = rules.values.toList.distinct.map(letter => {
      val value = state
        .collect(a => a._1.count(_.toString == letter) * a._2)
        .sum
      letter -> calcSize(origin, letter, value)
    })

    val max = r.maxBy(_._2)
    val min = r.minBy(_._2)

    max._2 - min._2
  }

  private def calcSize(origin: String, letter: String, value: BigInt) = {
    if (value == 1) value
    else if (origin.startsWith(letter) && origin.endsWith(letter)) (value + 2) / 2
    else if (origin.startsWith(letter) || origin.endsWith(letter)) (value + 1) / 2
    else value / 2
  }

  def mostCommonElement(s: State): Char = {
    'a'
  }

  def leastCommonElement(s: State): Char = {
    'b'
  }

  def countOccurrences(origin: String, pair: String) = {
    var count = 0;
    for (i <- 0 until origin.length) {
      if (origin.slice(i, i + 2) == pair)
        count += 1
    }

    BigInt(count)
  }
}
