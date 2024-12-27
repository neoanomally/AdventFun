package com.sandersme.advent.twentyfour

import Gate.*
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}
import com.sandersme.advent.Input
import com.sandersme.advent.twentyone.math.VectorDistance.normDotProduct

enum Gate {
  case AND, XOR, OR
}


// For each node I want to know I want to know what gate instructions it starts as a left
// of
// Also for each node i want to know which gates instructions it starts as an output to. 
// Then for each node I want to know which gates they are the output for?
// I don' tneed a leftMap and rightMap i just need to have an inMap and outMap... The
// 
case class NodeMap(leftMap: Map[String, List[GateInstruction]], rightMap: Map[String, List[GateInstruction]],
  outMap: Map[String, List[GateInstruction]]) {
    
    def addInstruction(instruction: GateInstruction): NodeMap = {
      val updatedLeft = updateMap(leftMap, instruction.leftWire, instruction)
      val updatedRight = updateMap(rightMap, instruction.rightWire, instruction)
      val updatedOut = updateMap(outMap, instruction.outWire, instruction)

      this.copy(updatedLeft, updatedRight, updatedOut)
    }

    private def updateMap(map: Map[String, List[GateInstruction]], key: String,
      instruction: GateInstruction): Map[String, List[GateInstruction]] = {

        val updatedValue = map.getOrElse(key, List.empty) :+ instruction
        map + (key -> updatedValue)
    }

    def findUpPathFromWire(wire: String): List[GateInstruction] = {
      if (outMap.contains(wire)) {
        val instructions = outMap(wire)
        val leftInstructions = instructions.map(inst => findUpPathFromWire(inst.leftWire)).flatten
        val rightInstructions = instructions.map(inst => findUpPathFromWire(inst.rightWire)).flatten
    
        instructions ++ leftInstructions ++ rightInstructions
      } else {
        List.empty
      }
    } 

    def findDownPathFromWire(wire: String, visited: Set[String] = Set.empty[String]): List[GateInstruction] = {
      val allInstructions = leftMap.getOrElse(wire, List.empty) ++ rightMap.getOrElse(wire, List.empty)

      if (allInstructions.isEmpty) {
        List.empty
      } else {
        allInstructions ++ allInstructions.flatMap(inst => findDownPathFromWire(inst.outWire))
      }
    }

    def findPathHopsFromWire(wire: String, level: Int = 0): Map[Int, List[GateInstruction]] = {
      if (outMap.contains(wire)) {
        val instruction = outMap(wire)
        val leftInstructions = instruction.map(instruction => findPathHopsFromWire(instruction.leftWire, level + 1))
        val rightInstructions = instruction.map(instruction => findPathHopsFromWire(instruction.rightWire, level + 1))

        (leftInstructions ++ rightInstructions).foldLeft(Map(level -> instruction)){ (baseMap, next) =>
          next.foldLeft(baseMap) { (mapAgg, map) => 
           
            val updatedVal = mapAgg.getOrElse(map._1, List.empty) ++ map._2
            mapAgg + (map._1 -> updatedVal)
          }
        }

      } else {
        Map.empty
      }
    }

    // FOR EACH NODE, I want to know the number of Dependency gates that exist.
    // AND for each node I want to know the earliest level that node exists. 
    def findNumberOfGatesPerNode: List[(String, List[Gate])] = {
      val distinctNodes =  leftMap.keySet ++ rightMap.keySet ++ outMap.keySet

      distinctNodes
        .map(node => node -> (leftMap.getOrElse(node, List.empty) ++ rightMap.getOrElse(node, List.empty)).map(_.gate))
        .map((node, list) => node -> list.sortBy(_.toString()))
        .toList
    }

}

object NodeMap { 
  def empty: NodeMap = {
    NodeMap(Map.empty, Map.empty, Map.empty)
  }
}

case class Computation(instruction: GateInstruction, outWire: String)
case class GateInstruction(leftWire: String, rightWire: String, gate: Gate, outWire: String)
case class CrossedWires(wires: Map[String, Boolean], gateInstructions: List[GateInstruction], nodeMap: NodeMap, original: List[GateInstruction]) {
  def swap(left: String, right: String): CrossedWires = {
    if (nodeMap.outMap.getOrElse(left, List.empty).size != 1 || nodeMap.outMap.getOrElse(right, List.empty).size != 1) {
      throw new Exception(f"ERROR ${left} or ${right} does not have only one output and not a valid option to swap")
    }

    val  updatedGateInstructions = gateInstructions.map{ instruction =>
      if (instruction.outWire == left) {
        println(f"Swapping ${left} with ${right} ${instruction} to ${instruction.copy(outWire = right)}")
        instruction.copy(outWire = right )
      } else if (instruction.outWire == right) {
        println(f"Swapping ${left} with ${right} ${instruction} to ${instruction.copy(outWire = left)}")
        instruction.copy(outWire = left)
      } else {
        instruction 
      }
    }

    val updatedNodeMap = CrossedWires.buildNodeMap(updatedGateInstructions)

    val updatedWires = wires.filter(wire => wire._1.startsWith("x") || wire._1.startsWith("y"))

    CrossedWires(wires, updatedGateInstructions, updatedNodeMap, original)
      .unconnectAllWires
      .connectAllWires
  }

  def findCandidateGates: Map[GateInstruction, Int] = {
    findIncorrectOutWires
      .flatMap(nodeMap.findUpPathFromWire)
      .groupMapReduce(identity)(_ => 1)(_ + _)
  } 

  def gateCurrentValues(gate: GateInstruction): Map[String, Boolean] = {
    Map(
      gate.leftWire -> wires(gate.leftWire),
      gate.rightWire -> wires(gate.rightWire),
      gate.outWire -> wires(gate.outWire)
    )
  }

  def findIncorrectOutWires: List[String] = {
    val expectedZBinary = java.lang.Long.toBinaryString( yLong + xLong).reverse
    val currZBinary = zBinary.reverse
    val wrongValues = for { 
      x <- 0 until expectedZBinary.length 
      
      if (expectedZBinary(x) != currZBinary(x))
    } yield x 

    wrongValues.toList
      .map(v => if (v < 10) ("z0" + v) else ("z" + v ))
  }


  def xBinary: String = {
    binaryString("x")
  }

  def xLong: Long = {
    java.lang.Long.parseLong(xBinary, 2)
  }

  def yLong: Long = { 
    java.lang.Long.parseLong(yBinary, 2)
  }

  def zLong: Long = {
    java.lang.Long.parseLong(zBinary, 2)
  }

  def yBinary: String = {
    binaryString("y")
  }

  def zBinary: String = {
    binaryString("z")
  }

  def binaryString(v: String): String = {
    wires.toList
      .filter(_._1.startsWith(v))
      .sortBy(_._1)
      .map { case (k, v) => 
        v match {
          case true => 1
          case false => 0
        }
      }.mkString
      .reverse
  }

  def unconnectAllWires: CrossedWires = {
    val updatedWires = wires.filter((wire, _) => wire.startsWith("x") || wire.startsWith("y"))

    this.copy(wires = updatedWires)
      .connectAllWires
  }

  def connectAllWires: CrossedWires = {
    // TOdo instead of doing a linear search I can have a map lookup to the nodes that are
    // waiting on a signal to open up. That way when I apply an instruction I can hop to
    // those waiting.  What we would do is create a map of all wires to all gates. That
    // way we can check that when a wire is connected we can see if all those have been
    // completed. 
      val updatedWires = loop(wires, gateInstructions)
      this.copy(wires = updatedWires)
  }

  def getZBinaryToLong: Long = {
    val binary = connectAllWires.zBinary

    java.lang.Long.parseLong(binary, 2)
  }

  val MAX_LOOPS = 2000
  @tailrec
  private def loop(currentWireMap: Map[String, Boolean], instructions: List[GateInstruction], loopCount: Int = 0): Map[String, Boolean] = {
    if (loopCount > MAX_LOOPS) {
      throw new Exception("ERROR WE LOOPED TOO MANY TIMES")
    }

    if(instructions.isEmpty || loopCount > MAX_LOOPS) {
      println(f"We looped ${loopCount} times")
      currentWireMap
    } else {
      val (skippedInstructions, updatedWires) = instructions
        .foldLeft((List.empty[GateInstruction], currentWireMap)){ case ((skippedAgg, mapAgg), next) =>
          
          if (currentWireMap.contains(next.leftWire) && currentWireMap.contains(next.rightWire)) {

            val gateResult = next.gate match {
              case OR => mapAgg(next.leftWire) || mapAgg(next.rightWire)
              case AND => mapAgg(next.leftWire) && mapAgg(next.rightWire) 
              case XOR => mapAgg(next.leftWire) ^ mapAgg(next.rightWire)
            }
            val updatedMapAgg = mapAgg + (next.outWire -> gateResult)

            (skippedAgg, updatedMapAgg)
          } else {
            (skippedAgg :+ next, mapAgg)
          }

      }

      loop(updatedWires, skippedInstructions, loopCount + 1)
    }
  }
}

object CrossedWires {
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyFourFromResource("day24_input")


    val startTime = System.currentTimeMillis()
    val crossedWires = CrossedWires.parseInput(input)
      .connectAllWires
    val totalTime = System.currentTimeMillis() - startTime

    val decimalValue = crossedWires.getZBinaryToLong

    // TROUBLESHOOOTING
    println("X:  " + crossedWires.xBinary)
    println("Y:  " + crossedWires.yBinary)
    println("\n\n")

    val expectedZLong = crossedWires.xLong + crossedWires.yLong
    val expectedZBinary = java.lang.Long.toBinaryString(expectedZLong)
    val zBinary = crossedWires.zBinary

    println("XZ: " + expectedZBinary)
    println("Z:  " + crossedWires.zBinary)

    // These gates are a great way to work backwards
    // One observation is that the final gates are all xor gates. 
    // What that tells us is that any upstream changes need to flip only one of the values
    // for each; which helps streamline the solution. We don't want both the left wire and
    // the right wire to swap outputs. 
    // WE can find the 8 gates that only change ONLY one of the input values, 
    // one way we can do this is to do a two hop 


    println(f"The decimal value for part 1 is ${decimalValue} and it took ${totalTime}ms")

    println("The problem  nodes: ")
    crossedWires.findIncorrectOutWires.foreach(println)
    // crossedWires.findIncorrectOutWires.foreach(println)
    // println("\n\n")

    val singleGates = crossedWires.nodeMap.findNumberOfGatesPerNode
      .sortBy(_._1)



    // println("SWAPPING hgv and ngv new incorrect wires") 
    val updatedCrossWires = crossedWires
      .swap("z21", "nks")
      .swap("z10", "gpr")
      .swap("z33", "ghp")
      .swap("krs", "cpm")
      

    println("THE UPDATED PROBLEM NODES:")
    updatedCrossWires.findIncorrectOutWires
      .foreach(println)


    val sortedResult = List("z21", "z10", "z33", "krs", "nks", "gpr", "ghp", "cpm")
      .sorted
      .mkString(",")
    println("The answer to part2: " + sortedResult)
    def makeTab(idnt: Int): String = (0 until idnt ).map(_ => "\t").mkString

/**
  * UNCOMMENT THE FOLLOWING TO HAND GO THORUGH THE NODES
  * What I found was that each node is going to have five reproducable steps. I didn't
  * code this up because I'd have to do a lot of refactoring
  * The five steps for each new node are as follows:
  * 1. take the Previous X#-1 AND Y#-1
  * 2. Take the previous input Z#-1 and AND those
  * 3. Take step1 OR step2
  * 4. Take current X# and Y# for Z#
  * 5. Take step3 XOR step4
  */
    // (38 until 42).foreach{ case idx =>
    //   val prefix = if (idx <= 9) "z0" else "z"
    //   val zIdx = prefix + idx
    //   println("\nGATE: " + zIdx)
    //   val paths = updatedCrossWires.nodeMap.findPathHopsFromWire(zIdx)
    //     .toList.sortBy(-_._1)
    //     .map(_._2)
    //     .zipWithIndex
    //     .map((list, idx) => (list, makeTab(idx)))
    //
    //   paths
    //     .foreach((list, tab) => println(tab+ list.toString))
    //
      // println("SIZE: " + paths.map(_._1.size).sum)
    // }


    println("Number of possible gates: " + crossedWires.gateInstructions.size)
  }


  def parseInput(in: List[String]): CrossedWires = {
    val startingWire = in.takeWhile(_.trim.nonEmpty)
      .map{line => 
        val split = line.split(": ")
        val value = split(1).toInt match {
          case 0 => false
          case 1 => true
          case _ => throw new Exception("Error splitting starting wires should not get here: " + split(1))
        }
        split(0) -> value
      }.toMap

    val gateInstructions = in.drop(startingWire.size + 1)
      .map { line =>
// x00 AND y00 -> z00
        val split = line.split(" ")
        val gate = split(1) match {
          case "AND" => AND
          case "OR" => OR
          case "XOR" => XOR
        }

        GateInstruction(split(0), split(2), gate, split(4)) 
      }
      
      val nodeMap = buildNodeMap(gateInstructions)
      CrossedWires(startingWire, gateInstructions, nodeMap, gateInstructions)
  }

  def buildNodeMap(instructions: List[GateInstruction]): NodeMap = {
    instructions.foldLeft(NodeMap.empty) { case (nodeMapAgg, instruction) =>
      nodeMapAgg.addInstruction(instruction)
    }
  }
}

