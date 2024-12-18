package com.sandersme.advent.twentyfour

import com.sandersme.advent.twentyfour.Register
import com.sandersme.advent.Input

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
      case 0 => adv(operand) // advance division on Numerator(A) / 2 ^ Combo Operand
      case 1 => bxl(operand) 
      case 2 => bst(operand)
      case 3 => jnz(operand)
      case 4 => bxc(operand)
      case 5 => out(operand)
      case 6 => bdv(operand)
      case 7 => cdv(operand)
      case _ => throw new Exception("Error should not reach this state in opcodeMatch") 
    }
  }

  // Opcode 0 
  def adv(operand: Int): ChronoSpatialComputer = {
    val updatedA =  (registers.A / (Math.pow(2, getComboOperand(operand)))).toInt 
    val updatedRegisters = registers.copy(A = updatedA)
    this.copy(registers = updatedRegisters)
  }

  def bxl(operand: Int): ChronoSpatialComputer = {
    val updatedB = registers.B ^ operand
    val updatedRegisters = registers.copy(B = updatedB)
    this.copy(registers = updatedRegisters)
  } 

  def bst(operand: Int): ChronoSpatialComputer = {
    val updatedB = getComboOperand(operand) % 8
    val updatedReg = registers.copy(B = updatedB) // TODO Not sure if overwrite or add here
    this.copy(registers = updatedReg)
  }

  // The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A register is not zero, it jumps by setting the instruction pointer to the value of its literal operand; if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.
  def jnz(operand: Int): ChronoSpatialComputer = {
    if(registers.A == 0) {
      this
    } else {
      // println(f"jumping to $operand")
      this.copy(idx = operand - 2) // TODO: Should we just update it to idx - 2 here? so that we can generalize adding above?
    }
  }

  def bxc(operand: Int): ChronoSpatialComputer = {
    val xor = registers.B ^ registers.C
    val updatedReg = registers.copy(B = xor) // Does this get added or replaceD?

    this.copy(registers = updatedReg)
  }

  def out(operand: Int): ChronoSpatialComputer = {
    val res = getComboOperand(operand) % 8
    this.copy(output = output :+ res)
  }

  def bdv(operand: Int): ChronoSpatialComputer = {
    val updatedB = (registers.A / (Math.pow(2, getComboOperand(operand)))).toInt
    val updatedReg = registers.copy(B = updatedB)
    this.copy(registers = updatedReg)
  }

  def cdv(operand: Int): ChronoSpatialComputer = {
    val updatedC = (registers.A / (Math.pow(2, getComboOperand(operand)))).toInt
    val updatedReg = registers.copy(C = updatedC)
    this.copy(registers = updatedReg)
  }

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
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyFourFromResource("day17_input")

    val computer = parseInput(input)

    val advancedStateComputer = advanceStateTillComplete(computer)
    println(advancedStateComputer.printOutput)
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
