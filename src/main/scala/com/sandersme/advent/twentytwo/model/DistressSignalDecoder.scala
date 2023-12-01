package com.sandersme.advent.twentytwo.model

import scala.annotation.tailrec


sealed trait PacketType
case class PacketValue(value: Int) extends PacketType {
  override
  def toString: String = value.toString
}
case class PacketList(values: List[PacketType]) extends PacketType {
  override
  def toString: String = '[' + values.mkString(",") + ']'
}

case object PacketTerminator extends PacketType
case class Packet(startingNode: PacketType)


case class PacketPair(left: PacketType, right: PacketType) {
  def isCorrectOrder: Boolean = ???

  override
  def toString: String = s"Left:\t${left}\nRight:\t${right}"
}


object PacketType {
  // We should have a pattern something like:
  //      [Items[]] A
  //      [Items[[]]] B
  // *BLANK*
  //      [Items[[[[]]]] A
  //      [Items[[]]] V
  // WE Have four states:
  // [    ->  Start Accumulating
  // ]    ->  Stop  Accumulating return PacketList(accumulated)
  // ,    -> Currently constructing a list, ignore
  // int  -> Currently Constructing a list append

 // Consume start, and then consume the string for the n
 //
  def parse(input: String): PacketType = {
    startParsing(input.toList, List.empty)
  }

   def startParsing(input: List[Char], accumulation: List[PacketType]): PacketType = {
     val results = findClosingBracket(input.toList, List.empty, List.empty)
//
//     if (remaining.isEmpty) {
//       println(s"RESULTS: ${results}")
//       val v = results match {
//         case PacketList(value) => value
//         case _ => throw new Exception("")
//       }
//
//       println(s"V: ${v}")
//       val preach = accumulation.head match {
//         case PacketList(value) => value ++ v
//       }
//
//       println(s"Preach: ${preach}")
//
//       PacketList(preach)
//     } else {
//       val updatedAccumulation = results match {
//         case PacketList(value) => accumulation ++ value
//         case _ => throw new Exception("OUTER LAYER SHOULD ALWAYS BE A LIST")
//       }
//       startParsing(remaining, updatedAccumulation)
//     }
    results
 }

  // Alternatively we have an accumulator and a result which is actually what we should
  // be doing anyways: , results: PacketType,
  //                          accumulated: List[PacketType]

   def findClosingBracket(input: List[Char],
                          packetAccumulation: List[PacketType],
                          result: List[PacketType]): PacketType = {
      input.headOption match {
        case Some('[') =>
            findClosingBracket(input.tail, List.empty, packetAccumulation ++ result)
        case Some(']') =>
          val packet = PacketList(packetAccumulation)
          if (input.tail.isEmpty && result.isEmpty) { // terminating case inside a nest
            packet
          } else if (input.tail.nonEmpty && result.isEmpty) { // This is inside a nest in the middle
            findClosingBracket(input.tail, List(packet), result)
          } else { // This is the case where we are in the middle of a list and pulling inner list

            val head::tail = result
            findClosingBracket(input.tail, List(head, packet), tail)
          }

        case Some(',') =>
          findClosingBracket(input.tail, packetAccumulation, result)

        case _ => // Should be numeric
          val int = input
           .takeWhile(value => value != '[' && value != ']' && value != ',')

          val remaining = input.drop(int.size)
          val updatedAccumulation = packetAccumulation :+ PacketValue(int.mkString.toInt)

          findClosingBracket(remaining, updatedAccumulation, result)
     }
 }

}

case class DistressSignalDecoder(packetPairs: List[PacketPair]) {
  def sumOfCorrectOrderIndicies: Int = ???
}
object DistressSignalDecoder {
  def parseInput(input: List[String]): DistressSignalDecoder = {
    val packetPairs = takeTwoPairs(input)

    DistressSignalDecoder(packetPairs)
  }

  @tailrec
  def takeTwoPairs(input: List[String],
                   packetPairs: List[PacketPair] = List.empty): List[PacketPair] = {
    if (input.isEmpty) {
      packetPairs
    } else if (input.head.isBlank) {
      takeTwoPairs(input.tail, packetPairs)
    } else {
      val left = PacketType.parse(input.head)
      val right = PacketType.parse(input(1))

      val remaining = input.drop(2)

      val packetPair = PacketPair(left, right)
      val updatedPacketPair = packetPairs :+ packetPair
      takeTwoPairs(remaining, updatedPacketPair)
    }

  }

}
