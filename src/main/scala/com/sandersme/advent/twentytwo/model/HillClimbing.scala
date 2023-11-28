package com.sandersme.advent.twentytwo.model

import scala.annotation.tailrec


case class HillPoint(x: Int, y: Int)

/**
 * I'm still not really clear the best way to have an immutable list of Nodes,
 * So the class has an immutable list to the location of the Nodes instead.
 */
case class Node(location: HillPoint, value: Char, neighbors: List[HillPoint]) {
  val DESTINATION_VALUE = 'E'

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

  def isStartingNode: Boolean = value == 'S'
  def isDestinationNode: Boolean = value == DESTINATION_VALUE

  /**
   * At any given point you can only move at most up one level. you can
   * Always move down any level.
   */
  def canMove(other: Char): Boolean = {
    val canReachDestination = value == 'z' && other == DESTINATION_VALUE
    val noMoreThanOneStepHigher = value + 1 - other >= 0
    noMoreThanOneStepHigher  || isStartingNode || canReachDestination
  }
}

case class HillGrid(grid: Vector[Vector[Node]]) {
  def getNode(hillPoint: HillPoint): Node = {
    grid(hillPoint.x)(hillPoint.y)
  }
}
object HillClimbing {

  def parseInput(input: List[String]): HillGrid = {
    val maxX = input.size
    val maxY = input.head.length

    val maxPoint = HillPoint(maxX, maxY)

    val vectorGrid: Vector[Vector[Node]] = input
      .zipWithIndex
      .map { (line, x) =>
        line.toVector
          .zipWithIndex
          .map{ (value, y) =>
            val hillPoint = HillPoint(x, y)
            val allNeighbors = findAllNeighbors(hillPoint, maxPoint)
            Node(hillPoint, value, allNeighbors)
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

    points
      .filter(point => point.x >= 0 && point.y >= 0)
      .filter(point => point.x < maxLocation.x && point.y < maxLocation.y)
      .filter(_ != location)
      .toList
  }

  @throws
  def findStartingNode(hillGrid: HillGrid): Node = {
    val result = hillGrid.grid
      .flatMap(_.find(_.isStartingNode))

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
  def findShortestPath(hillGrid: HillGrid): Int = {
    val startingPath = findStartingNode(hillGrid)

    val f = recursePaths(hillGrid, startingPath, Set(startingPath.location),
      List.empty, 0, List.empty)

    f.min
  }

  // TODO: It's not doing the search correctly. I'm an idiot.
  @tailrec
  def recursePaths(hillGrid: HillGrid, currentNode: Node, visited: Set[HillPoint],
                   toVisit: List[HillPoint], currentStep: Int,
                   allAttempts: List[Int]): List[Int] = {
    val newNodesToVisit = currentNode.neighbors
      .filterNot(visited.contains)
      .filterNot(toVisit.contains)
      .filter { point =>
        val node = hillGrid.getNode(point)
        currentNode.canMove(node.value)
      }

    val combinedToVisit = newNodesToVisit ++ toVisit

    if(currentNode.isDestinationNode) {
      allAttempts :+ currentStep
    } else if (combinedToVisit.isEmpty) {
      allAttempts :+ Integer.MAX_VALUE
    } else {
      val nextNode = hillGrid.getNode(combinedToVisit.head)
      val updatedToVisit = combinedToVisit.tail
      val updatedVisited = visited + nextNode.location

      recursePaths(hillGrid, nextNode, updatedVisited, updatedToVisit, currentStep + 1, allAttempts)
    }
  }
}
