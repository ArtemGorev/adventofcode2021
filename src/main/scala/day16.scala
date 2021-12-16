import java.lang.Integer.parseInt
import scala.io.Source

object day16 extends App {

  val hexadecimalCodes = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111"
  )

  sealed trait PacketInfo {
    val version: Int
    val typeId: Int
    val length: Int
  }


  val filename = "inputs/day16_0.txt"
  val source = Source.fromFile(filename).getLines().filterNot(_.isEmpty).mkString("")
  val packet = source.toList.map(hexadecimalCodes(_)).mkString("")
  val packetInfo = processPacket(packet)

  def getTypeId(packet: String, index: Int) = parseInt(packet.slice(index + 3, index + 6), 2)

  def getPacketVersion(packet: String, index: Int) = parseInt(packet.slice(index, index + 3), 2)

  def getLengthType(packet: String, index: Int) = parseInt(packet.slice(index + 6, index + 7), 2)

  def parseLiteralValue(packet: String, start: Int) = {
    var index = start + 6
    var isContinue = true
    var result = ""

    do {
      val x = packet.slice(index, index + 5)
      isContinue = x.startsWith("1")
      result = result ++ x.tail
      index = index + 5
    } while (isContinue)

    (BigInt(result, 2), index)
  }

  def getLengthOfSubPackets(packet: String, index: Int) = parseInt(packet.slice(index + 7, index + 22), 2)

  def getNumberOfSubPackets(packet: String, index: Int) = parseInt(packet.slice(index + 7, index + 18), 2)

  def processPacket(packet: String, index: Int = 0): PacketInfo = {
    val version = getPacketVersion(packet, index)
    val typeId = getTypeId(packet, index)
    if (typeId == 4) {
      val x = parseLiteralValue(packet, index)
      val packet1 = LiteralPacket(version, typeId, x._1, x._2 - index)
      println(packet1)
      packet1
    } else {

      val lengthType = getLengthType(packet, index)
      if (lengthType == 0) {
        val lengthOfSubPackets = getLengthOfSubPackets(packet, index)
        val offset = index + 7 + 15
        var packets = List[PacketInfo]()
        var internalIndex = offset

        do {
          val nextPacket = processPacket(packet, internalIndex)
          internalIndex += nextPacket.length
          packets = packets :+ nextPacket
        } while (internalIndex < offset + lengthOfSubPackets)

        val packet1 = OperatorPacket(version, typeId, lengthType, packets, lengthOfSubPackets + 7 + 15)
        println(packet1)
        packet1
      } else {
        var numberOfSubPackets = getNumberOfSubPackets(packet, index)
        val offset             = index + 7 + 11
        var internalIndex      = offset

        var packets = List[PacketInfo]()

        do {
          val nextPacket = processPacket(packet, internalIndex)
          internalIndex += nextPacket.length
          packets = packets :+ nextPacket
          numberOfSubPackets -= 1
        } while (numberOfSubPackets != 0)

        val packet1 = OperatorPacket(version, typeId, lengthType, packets, packets.map(_.length).sum + 7 + 11)
        println(packet1)
        packet1
      }
    }
  }


  def calcVersion(packetInfo: PacketInfo): Int = packetInfo match {
    case LiteralPacket(v, _, _, _) => v
    case OperatorPacket(v, _, _, packets, _) =>
      v + packets.map(calcVersion).sum
  }

  def gt(packets: List[PacketInfo]): Int = if (calc(packets.head) >  calc(packets.tail.head)) 1 else 0
  def lt(packets: List[PacketInfo]): Int = if (calc(packets.head) <  calc(packets.tail.head)) 1 else 0
  def eq(packets: List[PacketInfo]): Int = if (calc(packets.head) == calc(packets.tail.head)) 1 else 0

  def calc(packetInfo: PacketInfo): BigInt = packetInfo match {
    case LiteralPacket(_, _, value, _) => value
    case OperatorPacket(_, 0, _, packets, _) => packets.map(calc).sum
    case OperatorPacket(_, 1, _, packets, _) => packets.map(calc).product
    case OperatorPacket(_, 2, _, packets, _) => packets.map(calc).min
    case OperatorPacket(_, 3, _, packets, _) => packets.map(calc).max
    case OperatorPacket(_, 5, _, packets, _) => gt(packets)
    case OperatorPacket(_, 6, _, packets, _) => lt(packets)
    case OperatorPacket(_, 7, _, packets, _) => eq(packets)
  }

  case class LiteralPacket(version: Int, typeId: Int, value: BigInt, length: Int) extends PacketInfo

  case class OperatorPacket(version: Int, typeId: Int, lengthType: Int, subPackets: List[PacketInfo], length: Int)
    extends PacketInfo
  println(s"part1 => ${calcVersion(packetInfo)}")
  println(s"part2 => ${calc(packetInfo)}")

}
