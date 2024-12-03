package com.sandersme.advent.twentyfour

import com.sandersme.advent.Input

object RedNoseReports {
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyFourFromResource("day2_input")

    val parsedInput = input.map(parseLine)

    // val safeLines = parsedInput.map(line => isLineSafe(line))
    // val numSafe = countSafe(safeLines)

    val safeLinesWithRemoval = parsedInput.map{line => 
      val res = isLineSafe(line, isIgnored = true) || isLineSafe(line.tail, isIgnored = false)
      res
    }
    val numSafeWithRemoval = countSafe(safeLinesWithRemoval)
    
    print(f"Num Safe: $numSafeWithRemoval")
    // println(f"The number of safe Lines: $numSafe")
    // println(f"The number of safe lines with one removal: $numSafeWithRemoval")
  }


  def parseLine(line: String): List[Int] = {
    line
      .split(" ")
      .map(_.toInt)
      .toList
  }

  def isLineSafe(line: List[Int], idx: Int = 0, isPositive: Boolean = true, isIgnored: Boolean = false): Boolean = {
    val direction = if (idx != 0) isPositive else line(idx) - line(idx + 1) > 0
    
    if (idx >= line.length - 1) {
      // println(f"Solution for $line")
      true
    } else if (isSafe(line(idx), line(idx + 1), direction)) {
      isLineSafe(line, idx + 1, direction, isIgnored)
    } else if (isIgnored) {
      val updatedLine = line.patch(idx, Nil, 1)
      val alternateRemoval = line.patch(idx + 1, Nil, 1)
      // println(f"Dropping ${line(idx)} at idx: $idx new line: $updatedLine")
      // println(f"Droping ${line(idx - 1)} from $line which is idx: ${idx - 1}")
      // println(f"Droping ${line(idx)} from $line which is idx: ${idx}")
      val nextIdx = if (idx == 0) 0 else idx - 1
      isLineSafe(updatedLine, nextIdx, direction, false) || isLineSafe(alternateRemoval, idx, direction, false) 
    } else {
      // println(f"NO SOLUTION: $line")
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
