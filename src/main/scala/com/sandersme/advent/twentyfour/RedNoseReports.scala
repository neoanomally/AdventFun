package com.sandersme.advent.twentyfour

import com.sandersme.advent.Input

object RedNoseReports {
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyFourFromResource("day2_input")

    val parsedInput = input.map(parseLine)

    val safeLines = parsedInput.map(line => isLineSafe(line))
    val numSafe = countSafe(safeLines)

    val safeLinesWithRemoval = parsedInput.map(line => isLineSafe(line, isIgnored = true) || isLineSafe(line.tail))
    val numSafeWithRemoval = countSafe(safeLinesWithRemoval)
    
    println(f"The number of safe Lines: $numSafe")
    println(f"The number of safe lines with one removal: $numSafeWithRemoval")
  }


  def parseLine(line: String): List[Int] = {
    line
      .split(" ")
      .map(_.toInt)
      .toList
  }

  def isLineSafe(line: List[Int], idx: Int = 1, isPositive: Boolean = true, isIgnored: Boolean = false): Boolean = {
    val direction = if (idx != 1) isPositive else line(idx - 1) - line(idx) > 0
    
    if (idx >= line.length || (idx == line.length - 1 && isIgnored)) {
      true
    } else if (isSafe(line(idx - 1), line(idx), direction)) {
      isLineSafe(line, idx + 1, direction, isIgnored = isIgnored)
    } else if (isIgnored) {
      val updatedLine = line.drop(idx - 1)
      val otherUpdated = line.drop(idx)
      // println(f"Droping ${line(idx - 1)} from $line which is idx: ${idx - 1}")
      // println(f"Droping ${line(idx)} from $line which is idx: ${idx}")

      isLineSafe(updatedLine, idx, direction, isIgnored = false) || isLineSafe(otherUpdated, idx, direction, isIgnored = false)
    } else {
      false
    }
  }

  def countSafe(input: List[Boolean]): Int = {
    input.count(identity)
  }

  def isSafe(left: Int, right: Int, isPositive: Boolean): Boolean = {
    val diff = left - right
    val sameDirection = diff > 0 == isPositive
  
    Math.abs(diff) <= 3 && Math.abs(diff) > 0 && sameDirection 
  }
}
