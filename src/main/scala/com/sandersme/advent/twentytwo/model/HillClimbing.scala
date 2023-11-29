package com.sandersme.advent.twentytwo.model

import scala.annotation.tailrec

case class HillPoint(x: Int, y: Int)

enum NodeType {
  case Starting, Ending, Other
}

/**
 * I'm still not really clear the best way to have an immutable list of Nodes,
 * So the class has an immutable list to the location of the Nodes instead.
 */
case class Node(location: HillPoint, value: Char, neighbors: List[HillPoint], nodeType: NodeType) {
  /**
   * Given that we keep a list of Points on where they are located in the graph,
   * we need to quickly look up the Nodes.
   * @param hillGrid
   * @return
   */
  def getNeighborNodes(hillGrid: HillGrid): List[Node] = {
    neighbors
      .map(point => hillGrid.grid(point.x)(point.y))
  }

  def isStartingNode: Boolean = nodeType == NodeType.Starting
  def isDestinationNode: Boolean = nodeType == NodeType.Ending

  /**
   * At any given point you can only move at most up one level. you can
   * Always move down any level.
   */
  def canMoveFrom(other: Char): Boolean = {
    value + 1 - other >= 0
  }

  /**
   * This is used to go the opposite direction, if we wanted to go down one step at a time.
   */
  def canMoveTo(other: Char): Boolean = {
    other + 1 - value >= 0
  }
}

// TODO: Should just store starting and ending nodes as part of the class.
// Rewrite the E and S as a and z
case class HillGrid(grid: Vector[Vector[Node]]) {
  def getNode(hillPoint: HillPoint): Node = {
    grid(hillPoint.y)(hillPoint.x)
  }

  def findStartingNode: Node = {
    grid.flatMap(_.filter(_.isStartingNode)).head
  }

  def findEndingNode: Node = {
    grid.flatMap(_.filter(_.isDestinationNode)).head
  }
}
object HillClimbing {
  import scala.util.Random

  private val rand = new Random()
  private val runtime = Runtime.getRuntime
  import runtime.{totalMemory, freeMemory, maxMemory}

  def parseInput(input: List[String]): HillGrid = {
    val maxY = input.length
    val maxX = input.head.length

    val maxPoint = HillPoint(maxX, maxY)

    val vectorGrid: Vector[Vector[Node]] = input
      .zipWithIndex
      .map { (line, y) =>
        line.toVector
          .zipWithIndex
          .map{ (value, x) =>
            val hillPoint = HillPoint(x, y)
            val allNeighbors = findAllNeighbors(hillPoint, maxPoint)
            val (updatedValue, nodeType) = value match {
              case 'S' => ('a', NodeType.Starting)
              case 'E' => ('z', NodeType.Ending)
              case _ =>   (value, NodeType.Other)
            }

            Node(hillPoint, updatedValue, allNeighbors, nodeType)
          }.toVector
      }.toVector



    HillGrid(vectorGrid)
  }

  /**
   * Find all neighbors that aren't the current node and the node is valid.
   * A valid node is where the x, y coordinates are > 0 and < maxValue.
   * @param location
   * @param maxLocation
   * @return
   */
  def findAllNeighbors(location: HillPoint, maxLocation: HillPoint): List[HillPoint] = {
    val left = HillPoint(location.x - 1, location.y)
    val right = HillPoint(location.x + 1, location.y)
    val up = HillPoint(location.x, location.y + 1)
    val down = HillPoint(location.x, location.y - 1)

    val points = List(left, right, up, down)

    val results = points
      .filter(point => point.x >= 0 && point.y >= 0)
      .filter(point => point.x < maxLocation.x && point.y < maxLocation.y)
      .toList

    results
  }

  @throws
  def findStartingNode(hillGrid: HillGrid): Node = {
    val result = hillGrid.grid
      .flatMap(_.find(_.isStartingNode))

    if (result.size != 1)
      throw new Exception("Error could not find starting node")

    result.head
  }

  def findShortestPathFromStart(hillGrid: HillGrid): Int = {
    val startingNode = findStartingNode(hillGrid)
    val finalMap = findShortestPathFromPoint(hillGrid, startingNode)

    finalMap(findDestinationNode(hillGrid).location)
  }

  def findShortestAFromEndingNode(hillGrid: HillGrid): Int = {
    val startingNode = findDestinationNode(hillGrid)

    val map = Map(startingNode.location -> 0)

    val neighborsToVisit = startingNode
      .neighbors
      .map(hillGrid.getNode)
      .filter(node => node.canMoveFrom(startingNode.value))
      .map(_.location)

    val visited = Set(startingNode.location)

    val finalMap = programmingFromDestination(map, visited, hillGrid, startingNode, neighborsToVisit)

    finalMap
      .filter{ case (point, _) => hillGrid.getNode(point).value == 'a' }
      .values
      .min
  }

  def findShortestPathFromPoint(hillGrid: HillGrid, startingNode: Node): Map[HillPoint, Int] = {
    val map = Map(startingNode.location -> 0)
    val neighborsToVisit = startingNode
      .neighbors
      .map(hillGrid.getNode)
      .filter(node => node.canMoveFrom(startingNode.value))
      .map(_.location)

    val visited = Set(startingNode.location)

    val finalMap = programmingFromSource(map, visited, hillGrid, startingNode, neighborsToVisit)

    printFinalGridAndDistances(hillGrid, finalMap)

    finalMap
  }

  def printFinalGridAndDistances(hillGrid: HillGrid, finalMap: Map[HillPoint, Int]): Unit = {
    hillGrid.grid
      .foreach { v =>
        v.foreach { node =>
          val value = node.nodeType match {
            case NodeType.Other => node.value
            case NodeType.Ending => 'E'
            case NodeType.Starting => 'S'
          }

          print(s"${value} ")
        }
        println("")
      }

    println("\n\n")

    hillGrid.grid
      .foreach { v =>
        v.foreach { node =>
          val distance = finalMap.getOrElse(node.location, -5)
          print(s"${distance} ")
        }
        println("")
      }
  }

  // TODO: Only uphill neighbors and downhill neighbors?

  /**
   * This should only be called from another internal method.
   * @param map
   * @param visited
   * @param hillGrid
   * @param comingFrom
   * @param toVisit
   * @return
   */
  @tailrec
  private def programmingFromSource(map: Map[HillPoint, Int], visited: Set[HillPoint],
                                    hillGrid: HillGrid, comingFrom: Node,
                                    toVisit: List[HillPoint]): Map[HillPoint, Int] = {


    if (toVisit.isEmpty) {
      map
    } else {
      val current = toVisit.head
      val currentNode = hillGrid.getNode(current)

      val neighbors = currentNode
        .neighbors
        .map(hillGrid.getNode)

      val neighborsThatCouldVisitCurrent = neighbors
        .filter(node => node.canMoveFrom(currentNode.value))
        .map(_.location)

      val visitableNeighbors = neighbors
        .filter(node => currentNode.canMoveFrom(node.value))
        .map(_.location)

      val unvisitedNeighborsCanVisit = visitableNeighbors
        .filterNot(visited.contains)
        .filterNot(toVisit.contains)

      val distanceFromVisited = neighborsThatCouldVisitCurrent
        .flatMap(map.get).min + 1

      val updatedMap = map + (current -> distanceFromVisited)
      val updatedToVisit =  toVisit.tail ++ unvisitedNeighborsCanVisit
      val updatedVisited = visited + current

      programmingFromSource(updatedMap, updatedVisited, hillGrid, currentNode, updatedToVisit)
    }
  }

  // TODO: Clean this up we can merge the two together. The only difference is the function for
  //  picking neighbors.
  @tailrec
  private def programmingFromDestination(map: Map[HillPoint, Int], visited: Set[HillPoint],
                                    hillGrid: HillGrid, comingFrom: Node,
                                    toVisit: List[HillPoint]): Map[HillPoint, Int] = {
    if (toVisit.isEmpty) {
      map
    } else {
      val current = toVisit.head
      val currentNode = hillGrid.getNode(current)

      val neighbors = currentNode
        .neighbors
        .map(hillGrid.getNode)

      val neighborsThatCouldVisitCurrent = neighbors
        .filter(node => node.canMoveTo(currentNode.value))
        .map(_.location)

      val visitableNeighbors = neighbors
        .filter(node => currentNode.canMoveTo(node.value))
        .map(_.location)

      val unvisitedNeighborsCanVisit = visitableNeighbors
        .filterNot(visited.contains)
        .filterNot(toVisit.contains)

      val distanceFromVisited = neighborsThatCouldVisitCurrent
        .flatMap(map.get).min + 1

      val updatedMap = map + (current -> distanceFromVisited)
      val updatedToVisit = toVisit.tail ++ unvisitedNeighborsCanVisit
      val updatedVisited = visited + current

      programmingFromDestination(updatedMap, updatedVisited, hillGrid, currentNode, updatedToVisit)
    }
  }

  @throws
  def findDestinationNode(hillGrid: HillGrid): Node = {
    val result = hillGrid.grid
      .flatMap(_.find(_.isDestinationNode))

    if (result.size != 1)
      throw new Exception("Error could not find starting node")

    result.head
  }



  /**
   * First find the starting node. Then either do a DFS or BFS to find the shortest path through
   * the
   * @param hillGrid
   * @return
   */

  /// TODO: Start from the destination node then find all the neighbros steps
  def findShortestPath(hillGrid: HillGrid): Int = {
    val startingNode = findStartingNode(hillGrid)
    val destinationNode = findDestinationNode(hillGrid)
    val startingMap = Map(destinationNode.location -> 0)

    val f = recursePaths(hillGrid, startingNode, Set(startingNode.location), startingMap)

    f(startingNode.location)
  }

  def debugInfo(currentNode: Node, stepsMap: Map[HillPoint, Int]): Unit = {
    if (rand.nextInt(1000000) % 1000000 == 0) {
      val totalGB = totalMemory() / 1024 / 1024
      val memoryGB = freeMemory() / 1024 / 1024
      val percentageString = (memoryGB.toDouble / totalGB.toDouble).formatted("%.4f")
      println(s"At node: ${currentNode}")
      println(s"Free memory MB = ${memoryGB}\t using Approx: ${percentageString}")
      println(stepsMap)
    }

  }

  // TODO: Dynamic programming to start backwards from
  // TODO: In order to have tail recursion we need a better way to loop that
  // TODO: Memoization Steps to E from node. Find E from each Node and calculate
  // keeps track of current and previous states.
  def recursePaths(hillGrid: HillGrid, currentNode: Node, visited: Set[HillPoint],
                   stepsMap: Map[HillPoint, Int]): Map[HillPoint, Int] = {

    // debugInfo(currentNode, stepsMap)

    val neighborsToVisit = currentNode.neighbors
      .filterNot(visited.contains)
      .filter { point =>
        val node = hillGrid.getNode(point)
        currentNode.canMoveFrom(node.value)
      }

    if (neighborsToVisit.isEmpty) {
      // TODO; this is probably not the best way to handle this. Verify
      stepsMap
    } else {
      val accumulatorStart = (Integer.MAX_VALUE, stepsMap)
      // Return steps + mappity... This could probably be it's own function.
      val (steps, foldedMap) = neighborsToVisit
        .foldLeft(accumulatorStart) { case ((minSteps, mappity), next) =>
          if (mappity.contains(next)) {
            val stepsToDestination = Math.min(mappity(next) + 1, minSteps)
            (stepsToDestination, mappity)
          } else {
            val updatedVisited = visited + next
            val nextNode = hillGrid.getNode(next)
            val updatedMap = recursePaths(hillGrid, nextNode, updatedVisited, mappity)
            val valueOrMax = updatedMap.getOrElse(next, Integer.MAX_VALUE - 1)

            val stepsToDestination = Math.min(valueOrMax + 1, minSteps)

            (stepsToDestination, updatedMap)
          }
      }

      val finalMap = if (steps == Integer.MAX_VALUE) {
        foldedMap
      } else {
        foldedMap ++ Map(currentNode.location -> (steps))
      }

      finalMap
    }
  }
}
