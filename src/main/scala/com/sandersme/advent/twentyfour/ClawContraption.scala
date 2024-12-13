package com.sandersme.advent.twentyfour

import com.sandersme.advent.Input
import com.sandersme.advent.twentyfour.ClawMachine
import java.math.BigInteger
import scala.annotation.tailrec
import com.sandersme.advent.twentytwo.model.Monkey.multiplyBusiestMonkies
import scala.annotation.threadUnsafe

case class CPoint(x: BigInt, y: BigInt) {
  def add(other: CPoint): CPoint = {
    CPoint(this.x + other.x, this.y + other.y)
  }
}
case class ClawMachine(a: CPoint, b: CPoint, prize: CPoint) {
  /// TODO: Maybe there is some trick to find some factors that we could then optimize
  //for. Once we have those factors we can findOptimizations towards those factors
  def multiplyPrize: ClawMachine = {
    val prizeX = prize.x + BigInt(10000000000000L)
    val prizeY = prize.y + BigInt(10000000000000L)

    this.copy(prize = CPoint(prizeX, prizeY))
  }

  // HAd to look up this formula because I tried doing brute force finding all multiples and 
  // I even ddid GCD, GCF, Factors of prime numbers; but optimizing the problem across thre 
  // three variables with a closed solution was tractable. 
  def optimizeToPrizePart2: BigInt = {
    val left = ((prize.x * b.y) - (b.x * prize.y)).toDouble / ((a.x * b.y) - (b.x * a.y)).toDouble
    val right = ((prize.x * a.y) - (a.x*prize.y)).toDouble / ((b.x * a.y) - (a.x * b.y)).toDouble 

    if (left.isWhole && right.isWhole) {
      BigInt((left.toLong * 3) + right.toLong)
    } else { 
      BigInt(0)
    }
  }

  def optimizeToPrize: BigInt = {
    val x = ClawMachine.findCombinations(a.x, b.x, prize.x)
    val y = ClawMachine.findCombinations(a.y, b.y, prize.y)

    val minTokens = x
      .filter(y.contains)
      .map{ case(aTimes, bTimes) => 
        (aTimes * 3) + bTimes
      }.minOption
    
    minTokens.getOrElse(0)
  }


  def gcdX: BigInt = {
    ClawMachine.gcd(a.x, ClawMachine.gcd(b.x, prize.x))
  }
  
  def gcdY: BigInt = {
    ClawMachine.gcd(a.y, ClawMachine.gcd(b.y, prize.y))
  }

}

object ClawMachine {
  def gcd(a: BigInt, b: BigInt): BigInt = {
    if (b == 0) {
      a
    } else {
      gcd(b, a % b)
    }

  }
  

  val PRIMES = List(2, 3, 5, 7, 11, 13, 17, 19)

  @tailrec
  def findFactors(in: BigInt, res: List[BigInt] = List.empty): List[BigInt] = {

    val factor = PRIMES.find(prime => in % prime == 0)
    factor match {
      case None if in == 1 => res
      case None => res :+ in
      case Some(f) => {
        val remaining = in / f 
        findFactors(remaining, res :+ f)
      }
    }
  }

  def findCombinations(a: BigInt, b: BigInt, target: BigInt): Set[(BigInt, BigInt)] = {
    val results = for {
      x <- BigInt(0) to (target / a + BigInt(1))

      remaining = target - (a * x)
      
      y = remaining / b
      if ( remaining % b == 0  && y >= 0)
    } yield (x, y)

    results.toSet
  }
}

case class ClawContraption(machines: Vector[ClawMachine]) {
  def findAllOptimizations: BigInt = {
    machines.map(_.optimizeToPrize)
      .foldLeft(BigInt(0)) { case (sum, next) => sum + next }
  } 

  def findPart2: BigInt = {
    machines.map(_.multiplyPrize)
      .map(_.optimizeToPrizePart2)
      .foldLeft(BigInt(0)) { case (sum, next) => sum + next }
  }
}


object ClawContraption {
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyFourFromResource("day13_input") 
    val startTime = System.currentTimeMillis()
    val clawContraption = parseInput(input)
    val endtime = System.currentTimeMillis()
    
    println("The min number of tokens: " + clawContraption.findAllOptimizations)
    println("The min number of tokens part 2: " + clawContraption.findPart2)
    println(f"Total time to process both parts: ${endtime - startTime}ms")
  }


  def parseInput(in: List[String], accumulator: Vector[ClawMachine] = Vector.empty): ClawContraption = {
    if (in.isEmpty) {
      ClawContraption(accumulator)
    } else {
      val next = in.take(4)
      val remaining = in.drop(4)
      
      val A = parseClawMachine(next(0))
      val B = parseClawMachine(next(1))
      val prize = parseClawMachine(next(2))
      
      val updatedAccum = accumulator :+ ClawMachine(A, B, prize)

      parseInput(remaining, updatedAccum)
    }
  }
// Button A: X+94, Y+34
// Button B: X+22, Y+67
// Prize: X=8400, Y=5400
  def parseClawMachine(in: String): CPoint = {
    val rightHand = in.split(": ")(1) 
    val splitXY = rightHand.split(", ")

    val X = splitXY(0).drop(2).toInt
    val Y = splitXY(1).drop(2).toInt

    CPoint(X, Y)
  }
}
