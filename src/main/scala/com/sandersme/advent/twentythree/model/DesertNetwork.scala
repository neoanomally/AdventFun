package com.sandersme.advent.twentythree.model

import scala.annotation.tailrec

enum Direction {
  case Left, Right
}

case class DesertNetwork(directions: DesertDirections, graph: DesertGraph) {
  val START = "AAA"
  val EXIT = "ZZZ" 

  def findStepsToExit: Int = {
    def loop(steps: Int, currentNode: DesertNode, currentDirections: DesertDirections): Int = {
      if (currentNode.sourceNode == EXIT) {
        steps
      } else if (currentNode.isDeadEnd) {
        throw new Exception("ERROR WE HAVE ENTERED A DEAD END")
      } else if (steps >= 1_000_000) {
        throw new Exception("ERROR: Exiting early in case infinite loop. Fix code.")
      } else {
        val nextDirection = currentDirections.peek
        val updatedDirections = currentDirections.moveOneDirection
        val nextNodeKey = currentNode.nodeKeyByDirection(nextDirection)
        val nextNode = graph.findNode(nextNodeKey)

        loop(steps + 1, nextNode, updatedDirections) 
      }
    }

    val startingNode = graph.findNode(START)
    loop(0, startingNode, directions)
  }
  

  def findCyclesAndExitPoints: Vector[Int] = {
    val allStartingNodes = graph.nodes.filter(_.isGhostStartingNode)

    val allCycles: Vector[Int] = allStartingNodes.map(findCycleForNode)
    allCycles
  }

  def calculateStepsFromCycles: Long = {
    val primeFactors = findCyclesAndExitPoints
      .map(DesertNetwork.findPrimeFactors)

    primeFactors.flatten.distinct
      .map(_.toLong)
      .product

  }

  def findCycleForNode(startingNode: DesertNode): Int = {
      def loop(current: DesertNode, steps: Int,  exits: Map[String, Int], 
        curDirections: DesertDirections): Int = {
          val nextDirection = curDirections.peek
          val updatedDirections = curDirections.moveOneDirection
          val nextNodeKey = current.nodeKeyByDirection(nextDirection)
          val nextNode = graph.findNode(nextNodeKey)

          if (nextNode == startingNode) { 
            steps 
          } else if (exits.contains(current.sourceNode)) { 
            exits(current.sourceNode)
          } else if (steps >= 125_000) {
             exits.head._2
          } else if (current.isGhostExitNode) {
            val updatedExits = exits + (current.sourceNode -> steps)
            loop(nextNode, steps + 1, updatedExits, updatedDirections) 
          } else {
            loop(nextNode, steps + 1, exits, updatedDirections)
          }
            
        }

        loop(startingNode, 0, Map.empty, directions)
  }

  /**
    * Note this will not finish in the input data. I thought this was deceptively simple
    *
    * @return
    */
  @deprecated
  def findGhostStepsToExit: Int = {
    @tailrec
    def loop(steps: Int, currentNodes: Vector[DesertNode], curDirections: DesertDirections): Int = {
      val allAtExit = currentNodes.forall(_.isGhostExitNode)
      if (allAtExit) {
        steps
      } else if(steps >= 1_000_000_000) {
        throw new Exception("ERROR EXITING early in case infinite loop. fix code 39.")
      } else {
        val nextDirection = curDirections.peek
        val updatedDirections = curDirections.moveOneDirection

        val nextNodes = currentNodes
          .map{ node => 
            val nextNodeKey = node.nodeKeyByDirection(nextDirection)
            graph.findNode(nextNodeKey)
          }

        loop(steps + 1, nextNodes, updatedDirections)
      }
    }

    val allStartingNodes = graph.nodes.filter(_.isGhostStartingNode)
    loop(0, allStartingNodes, directions) 
  }
}

case class DesertGraph(nodes: Vector[DesertNode]) {
  /**
    * This value is used to create a hashmap to find the index where any
    * given node is found. 
    */
  private val indexMap: Map[String, Int] = nodes
    .zipWithIndex
    .map{ case(node, idx) => node.sourceNode -> idx }
    .toMap


  /**
    * Method uses the index above to find the sourceNode and hope to it.  
    *
    * @param node
    * @return
      */
  def findNode(node: String): DesertNode = {
    nodes(indexMap(node))
  }
}
case class DesertNode(sourceNode: String, left: String, right: String) {
  def isDeadEnd: Boolean = { 
    sourceNode == left && sourceNode == right
  }

  def nodeKeyByDirection(direction: Direction): String = {
    direction match {
      case Direction.Left => left
      case Direction.Right => right
    }
  }

  val isGhostStartingNode: Boolean = sourceNode.endsWith("A")
  val isGhostExitNode: Boolean = sourceNode.endsWith("Z")
}

case class DesertDirections(values: List[Direction]) {
  def moveOneDirection: DesertDirections = {
    DesertDirections(values.drop(1) :+ values.head)
  }

  def peek: Direction = {
    values.head
  }
}

object DesertNetwork {
  def parseInput(input: List[String]): DesertNetwork = {
    val directions = parseDirections(input.head)

    val desertGraph = parseGraph(input.drop(2))

    DesertNetwork(directions, desertGraph)
  }

  def parseDirections(input: String): DesertDirections = {
    val directions: List[Direction] = input.map {
      case 'L' => Direction.Left
      case 'R' => Direction.Right
      case _   => throw new Exception("ERROR should not reach this branch processing directions") 
    }.toList

    DesertDirections(directions)
  }
  // Method use to find all the prime factors of a number
  def findPrimeFactors(num: Int): List[Int] = {
    @tailrec
    def loop(currNum: Int, results: List[Int]): List[Int] = {
      if (currNum >= num) {
        if (results.isEmpty)
          List(currNum)
        else
          results
      } else if (num % currNum == 0) {
        loop(currNum + 1, results :+ currNum)
      } else {
        loop(currNum + 1, results)
      }
    }

    loop(2, List.empty)
  }




  def parseGraph(input: List[String]): DesertGraph = {
    val nodes = input.map { case line =>
      val split = line.split(" = ")
      val sourceNode = split(0)

      val leftRightSplit = split(1)
        .drop(1)
        .dropRight(1)
        .split(", ")

      DesertNode(sourceNode, leftRightSplit(0), leftRightSplit(1))
    }.toVector

    DesertGraph(nodes)
  } 
}
