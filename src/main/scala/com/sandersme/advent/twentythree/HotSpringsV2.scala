package com.sandersme.advent.twentythree

import HotPattern.*
import com.sandersme.advent.twentythree.model.Diagnostic
import com.sandersme.advent.Input

import scala.collection.mutable.{Map => MMap}

enum HotPattern {
  case Damaged, Operational, Unknown
}
case class HotStruction(pattern: List[HotPattern], damagedExpected: List[Int]) {
  def unfold: HotStruction = {


    val updatedPattern: List[HotPattern] = (0 until 5).flatMap(_ => pattern :+ Unknown)
      .toList.dropRight(1)
    val updatedDamagedExpected = (0 until 5).flatMap(v => damagedExpected).toList


    HotStruction(updatedPattern, updatedDamagedExpected)
  }

  override def toString(): String = {
    val patternString = pattern.map{ _ match
      case Damaged => '#'
      case Unknown => '?'
      case Operational => '.'
    }.mkString
    
    patternString + " " + damagedExpected
  } 

  def countNumTimes: Long = {
    val cache = MMap[(Int, Int, Long), Long]()

    // I need to count numContinuousSoFar
    // which corresponsds to the head
    // as we loop through the line. At any point we'll encounter one of three symbols:
    // Dmged, Undamaged and wildcard. Any time we encounter a wildcard we want to treat it
    // as a damaged or undamaged to count the possible ways
    //
    // Any time I'm currently in a continguous damaged counting htough I exit if I can't
    // count the current num continguous items.
    // ???.### 1,1,3
    def loopTwo(remaining: List[HotPattern], expected: List[Int], currSoFar: Long): Long = {

      lazy val results = if (remaining.isEmpty && (expected.isEmpty || (expected.size == 1 && expected.head == currSoFar))) {
        1L
      } else if (remaining.isEmpty) {
        0L
      } else if (expected.isEmpty) {
        if (remaining.exists(_ == Damaged)) 0L else 1L
      } else if (currSoFar == expected.head && (remaining.isEmpty || remaining.head == Operational || remaining.head == Unknown)) {
        loopTwo(remaining.tail, expected.tail, 0L)
      } else if (currSoFar == expected.head && remaining.head == Damaged) { 
        0L 
      } else if (currSoFar > 0 && remaining.head == Operational) {
        0L // We have failed and exit early
      } else if (currSoFar == 0 && remaining.head == Operational) {
        // Remove any additional undamaged
        loopTwo(remaining.dropWhile(_ == Operational), expected, 0L) 
      } else if (remaining.head == Unknown && currSoFar == 0L) {
        // With and without
        loopTwo(remaining.tail, expected, currSoFar + 1) + loopTwo(remaining.tail, expected, currSoFar)
      } else if (remaining.head == Damaged || remaining.head == Unknown) {
        loopTwo(remaining.tail, expected, currSoFar + 1)
      } else {
        throw new Exception(f"Failed in loop two should not get here remaining: ${remaining.size} head: ${remaining.head} dmgExpectedRemaining ${damagedExpected.size} currSoFar: $currSoFar")
      }
      
      cache.getOrElseUpdate((remaining.size, expected.size, currSoFar), results)
    }
    
    loopTwo(pattern, damagedExpected, 0L)
  }
}
case class HotSpringsV2(instructions: List[HotStruction]) {
  def countAll: Long  = {
    instructions
      .map(_.countNumTimes)
      .sum
  }

  def unfoldCountAll: Long = {
    instructions
      .map(_.unfold)
      .map(_.countNumTimes)
      .sum
  }
}

object HotSpringsV2 {
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyThreeFromResource("day12_input")

    val startTime = System.currentTimeMillis
    val parsedInput = parseInput(input)


    val parseTest = parseInput(TEST_INPUT)

    println(parseTest.instructions.head.unfold)
    val totalTime = System.currentTimeMillis - startTime 
    
    val totalOptions = parsedInput.countAll
    val startUnfolding = System.currentTimeMillis()
    val unfoldedTotalOptions = parsedInput.unfoldCountAll
    val totalUnfoldingTime = System.currentTimeMillis() - startUnfolding

    println(f"Total options: ${totalOptions} took ${totalTime}ms")

    println(f"Unfolding each instruction has a total options of: ${unfoldedTotalOptions} and took ${totalUnfoldingTime}ms")
  }

  
  // ???.### 1,1,3
  def parseInput(in: List[String]): HotSpringsV2 = {
    val structions = in.map{ line =>
      val split = line.split(" ") 
      val pattern = split(0).map { _ match
        case '?' => Unknown
        case '#' => Damaged
        case '.' => Operational
        case _ => throw new Exception("Error parsing patter unknown character in: " + line)
      }.toList

      val damagedExpected = split(1).split(",").map(_.toInt).toList

      HotStruction(pattern, damagedExpected)
    }


    HotSpringsV2(structions)
  }


  val TEST_INPUT = """???.### 1,1,3
                      .??..??...?##. 1,1,3
                      ?#?#?#?#?#?#?#? 1,3,1,6
                      ????.#...#... 4,1,1
                      ????.######..#####. 1,6,5
                      ?###???????? 3,2,1""".split("\n").toList.map(_.trim)
}
