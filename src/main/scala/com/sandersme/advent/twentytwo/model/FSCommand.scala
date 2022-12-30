package com.sandersme.advent.twentytwo.model

import scala.annotation.tailrec

sealed trait FSCommand
case class CD(dir: String) extends FSCommand
case class LS(files: List[FileType]) extends FSCommand
case object InvalidCommand extends FSCommand

object FSCommand {
  @tailrec
  def parseAllFSCommands(commandLines: List[String],
                         accum: List[FSCommand] = List.empty): List[FSCommand] = {
    if(commandLines.isEmpty) {
      accum
    } else {
      val (fsCommand, remainingLines) = parseNextCommandLine(commandLines)
      val updatedAccum = accum :+ fsCommand

      parseAllFSCommands(remainingLines, updatedAccum)
    }
  }

  /**
   * This parses the remaining lines and returns the next FSCommand and the
   * remaining lines after parsing the commands. Right now we are returning an FSCommand. But may want
   * to return invalid command
   *
   * @param commandLines
   * @return
   */
  def parseNextCommandLine(commandLines: List[String]): (FSCommand, List[String]) = {
    assert(commandLines.nonEmpty)
    val line = commandLines.head
    val remainingLines = commandLines.tail

    if (line.startsWith("$ cd")) {
      val dirName = line.substring(5)
      val cd = CD(dirName)

      (cd, remainingLines)
    } else if (line.startsWith("$ ls")) {
      val (ls, updatedRemainingLines) = parseListings(remainingLines)

      (ls, updatedRemainingLines)
    } else {
      (InvalidCommand, commandLines.tail)
    }
  }

  def parseListings(startingLines: List[String]): (LS, List[String]) = {
    val listingLines: List[FileType] = startingLines
      .takeWhile(!_.startsWith("$"))
      .map(FileType.parseFiletype)

    val remainingLines = startingLines.drop(listingLines.length)

    (LS(listingLines), remainingLines)
  }
}
