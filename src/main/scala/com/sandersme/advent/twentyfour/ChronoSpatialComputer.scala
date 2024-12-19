package com.sandersme.advent.twentyfour

import com.sandersme.advent.twentyfour.Register
import com.sandersme.advent.Input
import scala.runtime.stdLibPatches.language.`3.0`
import com.sandersme.advent.twentytwo.model.Monkey.multiplyBusiestMonkies
import scala.annotation.tailrec

case class Register(A: Int, B: Int, C: Int)

case class ChronoSpatialComputer(instructions: List[Int], idx: Int, registers: Register, output: List[Int], itr: Int, debug: Boolean = false) {
  // let's assume the length is 6 (0, 1, 2, 3, 4, 5) and the idx is 4 length goes to 5 and
  def canAdvance: Boolean = {
    val belowUpperBound = itr < 1000 // Manually hard code to prevent infinite loop
    val canAdvance = belowUpperBound && idx < instructions.length - 1// TODO Check if this should be length - 2
    // println("IDX at: " + idx + " can advance: " + canAdvance)
    
    if (debug && !canAdvance) {
      println(f"BelowUpperbound: $belowUpperBound.... idx: $idx")
    }
    canAdvance
  }


  // TODO start with a state and then run backwards. The state can be determined. where we
  // have registers. etc. The hard one is jumping around. 
  def advanceState: ChronoSpatialComputer = {
    if (idx >= instructions.length) {
      this 
    } else {

      val opcode = instructions(idx)
      val operand = instructions(idx + 1)

      val advanced = opcodeMatch(opcode, operand)

      val res = advanced.copy(idx = advanced.idx + 2, itr = advanced.itr + 1)
      if(debug) { println("New state: " + res) }

      res
    } 
  }

  def printOutput: String = output.mkString(",")

  def opcodeMatch(opcode: Int, operand: Int): ChronoSpatialComputer = {
    if (debug) {
      println(f"Opcode: ${opcode} and operand: ${operand}")
    }
    opcode match {
      case 0 => {
        val updatedA =  (registers.A / (Math.pow(2, getComboOperand(operand)))).toInt 
        this.copy(registers = registers.copy(A = updatedA))
      } 
      case 1 => {
        val updatedB = registers.B ^ operand
        this.copy(registers = registers.copy(B= updatedB))
      }
      case 2 => { 
        val updatedB = getComboOperand(operand) % 8
        this.copy(registers = registers.copy(B = updatedB))
      }
      case 3 if registers.A == 0 => this 
      case 3 => this.copy(idx = operand - 2)
      case 4 => {
        val xor = registers.B ^ registers.C
        this.copy(registers = registers.copy(B = xor))
      }
      case 5 => { this.copy(output = output :+ (getComboOperand(operand) % 8)) } 
      case 6 => {
        val updatedB = (registers.A / (Math.pow(2, getComboOperand(operand)))).toInt
        this.copy(registers = registers.copy(B = updatedB))
      } 
      case 7 => {
        val updatedC = (registers.A / (Math.pow(2, getComboOperand(operand)))).toInt
        this.copy(registers = registers.copy(C = updatedC))
      } 
      case _ => throw new Exception("Error should not reach this state in opcodeMatch") 
    }
  }
 // 2,4,  1,3,  7,5,  1,5,  0,3,  4,3,  5,5,  3,0
  // Opcode 0 
  // 1. (2, 4) -> takes A % 8 and saves it to B. B will always be 0 - 7. 
  // 2. (1, 3) -> takes B that was just set from 1. and xor it with 3 - output 3, 2, 1, 0, 7, 6, 5, 4 into reg B       
  // 3. (7, 5) -> Takes A / Math.pow(2, B) and saves it to C  Math.pow(2, B) is 1, 2, 4,
  //    8, 16, 32, 64, 128.... So A will divide by that value and save it to reg C 
  // 4. (0, 3) -> takes A / 8.0 and then stores it into register A 
  // 5. (4 3) -> takes register B ^ C --- we know that B is always 0 - 7
  // 6  (5, 5) -> Takes register B % 8 and outputs the value to the terminal. 
  // 7.(3, 0) -> exits the program if it reaches there whenever A '0' if A does not equal
  //    zero it jumps back to step 1. While (A != 0)
  //
  //


   // WE need to solve for A 
   // 5 
   // 2, 4, 1, 3, 7, 5, 1, 5, 0, 3, 4, 3, 5, 5, 3, 0 
   // To get 0 
   //
   // B = 0 
   // A = 0  = A * 8 
   // B = 5 = B ^ 5  
   // C = 0 = A * Math.(2, 5)
   // B = 6 = B ^ 3
   // B = 0 A % 8


  // The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A register is not zero, it jumps by setting the instruction pointer to the value of its literal operand; if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.


  def getComboOperand(operand: Int): Int = {
    if (operand <= 3) {
      operand
    } else if (operand == 4) {
      registers.A
    } else if (operand == 5) {
      registers.B
    } else if (operand == 6) {
      registers.C
    } else  {
      throw new Exception("error should not reach this value")
    }
  }
  
}

object ChronoSpatialComputer {
  def solveForAll(input: List[Int]): List[Long] = {
    input.reverse.foldLeft((List[Long](0), 1)){ case ((aCandidates, step), value) => 
      val cands = aCandidates.flatMap(candidate => solveValuesStep(candidate, value))

      println("Candidates for step: " + step + " - "  + cands)
      (cands, step + 1)
    }._1
  }


  def solveValuesStep(in: Long, valueToCreate: Long): List[Long] = {
    val candidates = candidatesForA(in)
    solveBackwards(in, valueToCreate, candidates)
  }

  def solveBackwards(in: Long, value: Long, candidates: Seq[Long], idx: Int = 0): List[Long] = {
    candidates.map { AA =>
      val BB = (AA % 8) ^ 3
      val CC = (AA * Math.pow(2, BB)).toInt
      val OUTBB = ((BB ^ CC) ^ 5)

      if (runOneStep(AA, OUTBB, CC) == value)
        AA
      else
        -1
    }.filter(_ != -1).toList
  }

  def candidatesForA(in: Long): Seq[Long] = {
    (0 until 8).map(i => (in * 8) + i)
  }

  def runOneStep(aIn: Long, bIn: Long, cIn: Long): Long = {
    var a = aIn;  var b = bIn; var c  = cIn
    b = a % 8
    b = b ^ 3
    c = (a / Math.pow(2, b)).toLong
    a = a / 8
    b = ((b ^ c) ^ 5) 
    b % 8
  }

  def solve(in: Int): Unit = {
    var A = in
    var B = 0 
    var C = 0

    while (A != 0) {                  // 3, 0 
      B = A % 8                       // (2, 4)
      B = B ^ 3                       // 1, 3
      C = (A / Math.pow(2, B)).toInt  // 7, 5 -- C = A / Math.pow(2, B)
      B = B ^ 5                       // 1, 5
      A = A / 8                       // 1, 5 --
      B = B ^ C   
      print((B % 8) + " ")           // This should just be an identity  
    }
  }
  
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyFourFromResource("day17_input")

    val computer = parseInput(input)

    val advancedStateComputer = advanceStateTillComplete(computer)
    println("OUTPUT FROM input of: " + computer.registers.A + "\t" + advancedStateComputer.printOutput)
    // Validated this works
    println("LOOKING FOR SOLUTION TO: " + solveForAll(List(2,4,1,3,7,5,1,5,0,3,4,3,5,5,3,0)))
  }

  def advanceStateTillComplete(computer: ChronoSpatialComputer): ChronoSpatialComputer = {
    if (computer.canAdvance) {
      advanceStateTillComplete(computer.advanceState)
    } else {
      computer
    }
  }
  
// Register A: 729
// Register B: 0
// Register C: 0
//
// Program: 0,1,5,4,3,0 
  def parseInput(in: List[String]): ChronoSpatialComputer = {
    val registersList = in.take(3)
      .map(_.split(": "))
      .map(_(1).toInt)

    val register = Register(registersList(0), registersList(1), registersList(2)) 

    val program = in.last
      .split(": ")(1)
      .split(",")
      .toList
      .map(_.toInt)

      ChronoSpatialComputer(program, 0, register, List.empty, 0)
  }

}
