import day16.packet

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
  case class LiteralPacket(version: Int, typeId: Int, value: Int, length: Int) extends PacketInfo
  case class OperatorPacket(version: Int, typeId: Int, lengthType: Int, subPackets: List[PacketInfo], length: Int)
      extends PacketInfo

  val filename = "inputs/day16_0.txt"
  val source   = Source.fromFile(filename)
  val packet   = source.toList.map(hexadecimalCodes(_)).mkString("")

  def getPacketVersion(packet: String, index: Int) = parseInt(packet.slice(index, index + 3), 2)
  def getTypeId(packet: String, index: Int)        = parseInt(packet.slice(index + 3, index + 6), 2)

  def getLengthType(packet: String, index: Int)         = parseInt(packet.slice(index + 6, index + 7), 2)
  def getLengthOfSubPackets(packet: String, index: Int) = parseInt(packet.slice(index + 7, index + 22), 2)
  def getNumberOfSubPackets(packet: String, index: Int) = parseInt(packet.slice(index + 7, index + 18), 2)

  def parseLiteralValue(packet: String, start: Int) = {
    println("parseLiteralValue")
    var index      = start + 6
    var isContinue = true
    var result     = ""

    do {
      val x = packet.slice(index, index + 5)
      isContinue = x.startsWith("1")
      result = result ++ x.tail
      index = index + 5
    } while (isContinue)

    (parseInt(result, 2), index)
  }

  def processPacket(packet: String, index: Int = 0): PacketInfo = {
    println(index)
    println("processPacket")
    val version = getPacketVersion(packet, index)
    val typeId  = getTypeId(packet, index)
    if (typeId == 4) {
      val x = parseLiteralValue(packet, index)
      LiteralPacket(version, typeId, x._1, x._2 - index)

    } else {
      println("process OPERATOR")
      val lengthType = getLengthType(packet, index)
      if (lengthType == 0) {
        val lengthOfSubPackets = getLengthOfSubPackets(packet, index)
        val offset             = index + 7 + 15
        var packets            = List[PacketInfo]()
        var internalIndex      = offset

        do {
          val nextPacket = processPacket(packet, internalIndex)
          internalIndex += nextPacket.length
          packets = packets :+ nextPacket
        } while (internalIndex < offset + lengthOfSubPackets)

        OperatorPacket(version, typeId, lengthType, packets, packet.length)
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

        OperatorPacket(version, typeId, lengthType, packets, packet.length)
      }
    }
  }

  def calcVersion(packetInfo: PacketInfo): Int = packetInfo match {
    case LiteralPacket(v, _, _, _) => v
    case OperatorPacket(v, _, _, packets, _) =>
      v + packets.map(calcVersion).sum
  }

//  11101110000000001101010000001100100000100011000001100000

//  11101110000000001101010000001100100000100011000001100000
//                                 0100000100011000001100000

//  "1101000101001010010001001000000000"
//  "110" - version
//     "100" - typeId = 4
//        "01010" - A

  println(calcVersion(processPacket(packet)))

}
