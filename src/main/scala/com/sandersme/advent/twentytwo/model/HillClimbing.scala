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
   * Find all neighborNodes that aren't the current node and the node is valid.
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

  def findShortestPathFromStart(hillGrid: HillGrid): Int = {
    val startingNode = hillGrid.findStartingNode
    val finalMap = findShortestPathFromPoint(hillGrid, startingNode, true)

    finalMap(hillGrid.findEndingNode.location)
  }

  def findShortestAFromEndingNode(hillGrid: HillGrid): Int = {
    val startingNode = hillGrid.findEndingNode

    val finalMap = findShortestPathFromPoint(hillGrid, startingNode, false)

    finalMap
      .filter{ case (point, _) => hillGrid.getNode(point).value == 'a' }
      .values
      .min
  }

  def findShortestPathFromPoint(hillGrid: HillGrid, startingNode: Node, isFrom: Boolean)
  : Map[HillPoint, Int] = {
    val map = Map(startingNode.location -> 0)
    val neighborsToVisit = startingNode
      .neighbors
      .map(hillGrid.getNode)
      .filter(node => canMoveFn(node, startingNode.value, true))
      .map(_.location)

    val finalMap = programmingFromSource(map, hillGrid, neighborsToVisit, isFrom)

    // printFinalGridAndDistances(hillGrid, finalMap)

    finalMap
  }

  // This is a nice helper function that visualizes the graph.
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

  def canMoveFn(node: Node, char: Char, isFrom: Boolean): Boolean = {
    if (isFrom) {
      node.canMoveFrom(char)
    } else {
      node.canMoveTo(char)
    }
  }

  /**
   * This should only be called from another internal method.
   * @param map        This is used to hold not only which nodes we have visited but also how
   *                   many steps each node is.
   * @param hillGrid   This is the grid that holds our nested nodes.
   * @param toVisit These are the nodes left to visit. THis is a queue where we visit each node
   *                exactly once.
   * @param isUphill This is to tell the moveFn which direction we need to check if a node can move
   *               This is useful for going Uphill or down hill
   * @return
   */
  @tailrec
  private def programmingFromSource(map: Map[HillPoint, Int], hillGrid: HillGrid,
                                    toVisit: List[HillPoint],
                                    isUphill: Boolean): Map[HillPoint, Int] = {
    if (toVisit.isEmpty) {
      map
    } else {
      val currentNode = hillGrid.getNode(toVisit.head)

      val neighborNodes = currentNode
        .neighbors
        .map(hillGrid.getNode)

      val neighborsThatCouldVisitCurrent = neighborNodes
        .filter(node => canMoveFn(node, currentNode.value, isUphill))
        .map(_.location)

      val unvisitedNeighborsCanVisit = neighborNodes
        .filter(node => canMoveFn(node, currentNode.value, !isUphill))
        .map(_.location)
        .filterNot(map.contains)
        .filterNot(toVisit.contains)

      // We should always have at least one value given that the map is already prepopulated
      val distanceFromVisited = neighborsThatCouldVisitCurrent
        .flatMap(map.get).min + 1

      val updatedMap = map + (currentNode.location -> distanceFromVisited)
      val updatedToVisit =  toVisit.tail ++ unvisitedNeighborsCanVisit

      programmingFromSource(updatedMap, hillGrid, updatedToVisit, isUphill)
    }
  }

}
