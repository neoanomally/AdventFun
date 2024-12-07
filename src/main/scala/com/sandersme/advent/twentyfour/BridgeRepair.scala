package com.sandersme.advent.twentyfour

import com.sandersme.advent.twentyfour.BridgeRepair
import com.sandersme.advent.twentyfour.RepairInstruction
import com.sandersme.advent.twentyfour.BridgeRepair.combinatorialAddMul
import com.sandersme.advent.Input


case class BridgeRepair(instructions: List[RepairInstruction]) {
  def sumCanEqual: Long = {
    instructions
      .filter(_.leftCanEqualRight)
      .map(_.total)
      .sum
  }
}

case class RepairInstruction(total: Long, values: List[Long]) {
  def leftCanEqualRight: Boolean = { 
    combinatorialAddMul(values, total)
      .exists(_ == total)
  }
}

object BridgeRepair {
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyFourFromResource("day7_input")

    val bridgeRepair = BridgeRepair.parseInput(input)
    val sumPartOne = bridgeRepair.sumCanEqual

    println(f"THe sum of all valid instructions: $sumPartOne")


  }

  def combinatorialAddMul(in: List[Long], filterVal: Long, withConcat: Boolean = true): List[Long] = {
    in.tail.foldLeft(List(in.head)) { (accum, next) =>
      val addition = accum.map(_ + next)
      val multiply = accum.map(_ * next)

      val concat = if (withConcat) {
        accum.map(value => moveConcat(value, next))
      } else {
        List.empty
      }

      (addition ++ multiply ++ concat).filter(_ <= filterVal)
    }
  }

  def moveConcat(left: Long, right: Long): Long = {
    val moved = (0 until Math.floor(Math.log10(right) + 1).toInt)
      .foldLeft(left){ case (next, _) => 
        next * 10
      }
  
    moved + right
  }

  def parseInput(in: List[String]): BridgeRepair = {
    val instructions = in.map(parseInstruction)

    BridgeRepair(instructions)
  }

  def parseInstruction(line: String): RepairInstruction = {
      // format -> 190: 10 19
      val split = line.split(": ")
      val left = java.lang.Long.parseLong(split(0).trim)

      val right = split(1)
        .split(' ')
        .toList
        .map(_.trim)
        .map(java.lang.Long.parseLong)

      RepairInstruction(left, right)
    
  }
}
