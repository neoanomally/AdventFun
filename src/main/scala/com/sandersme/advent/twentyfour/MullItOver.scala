package com.sandersme.advent.twentyfour

import com.sandersme.advent.twentyfour.MulState
import com.sandersme.advent.twentyfour.Empty
import com.sandersme.advent.twentyfour.FirstInt
import com.sandersme.advent.twentyfour.SecondInt
import com.sandersme.advent.twentyfour.Skip
import com.sandersme.advent.Input

sealed trait MulState
case object Empty extends MulState

case object Skip extends MulState

case class FirstInt(x: Int) extends MulState {
  def append(v: Int): FirstInt = FirstInt((x * 10) + v)
} 

case class SecondInt(y: Int) extends MulState {
  def append(v: Int): SecondInt = SecondInt((y * 10) + v)
}


case class MatMul(left: Int, right: Int)
object MulProcessor {
  def create(line: String): MulProcessor = {
    val processor = MulProcessor(line, List.empty, 0, Empty, Empty)
    processAll(processor)
  }

  private def processAll(processor: MulProcessor): MulProcessor = {
    if (processor.hasNext) {
      processAll(processor.next)
    } else {
      processor
    }
  }
}
case class MulProcessor(line: String, matMuls: List[MatMul], idx: Int, prevState: MulState, currentState: MulState) {
  override def toString: String = f"$idx Prev: $prevState Current: $currentState"
  def hasNext: Boolean = idx < line.length()

  def next: MulProcessor = {
    val currentValue = line(idx)

    currentState match {
      case Skip if currentValue == 'd' => {
        if (line.substring(idx, idx + 4) == "do()") {
          this.copy(idx = idx + 4, currentState = Empty)
        } else {
          this.copy(idx = idx + 1)
        }
      }
      case Skip => this.copy(idx = idx + 1)
      case state: FirstInt if currentValue.isDigit => this.copy(currentState = state.append(currentValue.asDigit), idx = idx + 1) 
      case state: FirstInt if currentValue == ',' => this.copy(prevState = currentState, currentState = SecondInt(0), idx = idx + 1)
      case state: SecondInt if currentValue.isDigit => this.copy(currentState = state.append(currentValue.asDigit), idx = idx + 1) 
      case state: SecondInt if currentValue == ')' => 
        this.copy(matMuls = matMuls :+ createMatMul(prevState, state), idx = idx + 1, prevState = Empty, currentState = Empty ) 
      case _ if currentValue == 'm' => {
        currentState match {
          case _ => print("")
        }
        if (line.substring(idx, idx + 4) == "mul(") {
          this.copy(idx = idx + 4, currentState = FirstInt(0))
        } else {
          this.copy(idx = idx + 1, currentState = Empty, prevState = Empty)
        }
      }
      case _ if currentValue == 'd' => {
        if (line.substring(idx, idx + 7) == "don't()") {
          this.copy(idx = idx + 7, currentState = Skip)
        } else if (line.substring(idx, idx + 4) == "do()") {
          this.copy(idx = idx + 1)
        } else {
          this.copy(idx = idx + 1)
        }
      }
      case _ => this.copy(currentState = Empty, prevState = Empty, idx = idx + 1) 
    }
  }

  def sum: Int = {
    matMuls.filter(matMul => matMul.left < 1000 && matMul.right < 1000)
      .map(matMul => matMul.left * matMul.right).sum
  }

  def createMatMul(left: MulState, right: SecondInt): MatMul = {
    left match {
      case FirstInt(v) => MatMul(v, right.y)
      case _ => throw new Exception(f"Error should not get here at idx: $idx ${line(idx)}    current: $currentState  &  prev: $prevState")
    }
  }

}

object MullItOver {
  def main(args: Array[String]): Unit = { 
    val input = Input.readTwentyFourFromResource("day3_input")
      .mkString("")
    val sum = MulProcessor
      .create(input)
      .sum

    println(f"The sum for mull it over is: $sum")
  }

}
