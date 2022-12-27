package com.sandersme.advent.twentytwo.model

import scala.annotation.tailrec

object CommDecoder {
  val MESSAGE_POSITION = 14
  val START_OF_PACKET = 4

  def findStartOfMessage(line: String): Int = {
    loopIncrementCharacters(line, MESSAGE_POSITION, MESSAGE_POSITION)
  }

  /**
   * Start of packet is defined as the first time that there are
   * four unique characters in a row. This should be pretty easy by looping
   * through the bits until there are four unique in a row.
   * @param line this is the input string to find the first unique position
   * @return
   */
  def findStartOfPacket(line: String): Int = {
    loopIncrementCharacters(line, START_OF_PACKET, START_OF_PACKET)
  }


  /**
   * TODO: Include early stopping when the position is greater than the length of the entire input
   * @param line this is the packet that we are decoding.
   * @param position the current position that we are at. This is always going to start at the width..
   * @param width the width of the packet marker.
   * @return
   */
  @tailrec
  def loopIncrementCharacters(line: String, position: Int, width: Int): Int = {
    val indexEnd = position
    val indexStart = position - width
    val lastWidthChars = line.substring(indexStart, indexEnd)

    if (isWidthUnique(lastWidthChars, width))
      position
    else
      loopIncrementCharacters(line, position + 1, width)
  }


  def isWidthUnique(chars: String, width: Int): Boolean = {
    assert(chars.length == width)
    chars.toSet.size == width
  }
}
