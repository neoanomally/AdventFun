package com.sandersme.advent.model

import com.sandersme.advent.graph.{Grid, GridNode, Point}
import com.sandersme.advent.model.ChitonPath.ChitonRoute


import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable

/**
 * I have a feeling we are going to need the exact directions but for now we'll jus
 * calculate the shortest distance which is probably the simplist thing to do right now
 *
 * There is no reason to keep shipping the matrix
 */
case class ChitonPath(matrix: ChitonMatrix,
                      queuedRoutes: mutable.PriorityQueue[ChitonRoute],
                      visitedMap: VisitedMap) {

  override def toString: String = s"Routes: $queuedRoutes\t Visited Map: $visitedMap"

  def startNode: GridNode = matrix.head.head
  def endNode: GridNode = matrix.last.last
  def currentRoute: ChitonRoute = queuedRoutes.head
  def currentCumulativeDistance: Int = currentRoute.cost
  def currentPoint: Point = currentRoute.location
  def isAtEndNode: Boolean = currentPoint == endNode.location

  private[model] def visitedPointCost(point: Point): Int = visitedMap(point)

  def hasVisitedCurrentPoint: Boolean = visitedMap.contains(currentPoint)

  def currentVisitableNeighbors: List[GridNode] = {
    matrix(currentPoint.y)(currentPoint.x)
      .neighbors
      .filter(point => !visitedMap.contains(point))
      .map(point => matrix(point.y)(point.x))
  }

  def distanceToEnd: Option[Int] = distanceToPoint(endNode.location)
  def distanceToPoint(point: Point): Option[Int] = visitedMap.get(point)
}

type ChitonMatrix = Vector[Vector[GridNode]]
type VisitedSet = Set[Point]
type VisitedMap = Map[Point, Int]

object ChitonPath {
  case class ChitonRoute(location: Point, cost: Int)
  /**
   * We always start off on the left corner node (0, 0),
   * We need to visit our neighbors keeping a tally of whether or not
   * the neighbor has been visited. We only want to visit neighbors we haven't visited
   * We need to select the neighbor with the shortest path. We can do this with a Queue / PriorityQueue
   * That way we are taking from the top of the queue the shortest path each step of the way.
   *
   * As we travel through looking for the shortest path, we need to pop items off the queue, and into a backtracking
   * Stack/Queue as well. That way as we go back one step we can update our visited status and
   * remember which direction we came from.
   *
   * We need to keep track of the shortest path total, This will be something we
   * compare other paths to.
   *
   * TODO: When you back out of an area the area you just backed out of needs to add it's
   * Node in visited.
   *
   * @param chitonPath
   * @return
   */


  @tailrec
  def findShortestRoute(chitonPath: ChitonPath): ChitonPath = {
    val updatedVisited: VisitedMap = chitonPath.visitedMap +
      (chitonPath.currentPoint -> chitonPath.currentCumulativeDistance)

    val routesMinusCurrent = chitonPath.queuedRoutes.tail

    if (chitonPath.queuedRoutes.isEmpty || chitonPath.isAtEndNode) {
      chitonPath.copy(visitedMap = updatedVisited)
    } else if(chitonPath.hasVisitedCurrentPoint){
      val updatedPointCostToNode =
        Math.min(chitonPath.currentCumulativeDistance, chitonPath.visitedPointCost(chitonPath.currentPoint))

      findShortestRoute(chitonPath.copy(visitedMap = updatedVisited, queuedRoutes = routesMinusCurrent))
    }else {
      val neighborsCurrentLocations = chitonPath.currentVisitableNeighbors
        .map{neighborNode =>
          val updatedCost = neighborNode.value + chitonPath.currentCumulativeDistance
          ChitonRoute(neighborNode.location, updatedCost)
        }

      val updatedQueue = routesMinusCurrent ++ neighborsCurrentLocations

      findShortestRoute(chitonPath.copy(queuedRoutes = updatedQueue, visitedMap = updatedVisited))
    }
  }


  def parseInput(input: List[String]): ChitonPath = {
    val vectorGrid: Vector[Vector[Int]] =
      input.map(_.toArray.map(_.asDigit).toVector).toVector

    val maxX = vectorGrid.head.size
    val maxY = vectorGrid.size

    val matrix = vectorGrid.zipWithIndex.map{case (row, y) =>
      row.zipWithIndex.map { case (value, x) =>
        val neighbors = Grid.generateNeighborsMinusDiagnonals(x, y, maxX, maxY)
        GridNode(value, neighbors, Point(x, y))
      }
    }

    val defaultRoute = ChitonRoute(Point(0, 0), 0)

    val priorityQueue: mutable.PriorityQueue[ChitonRoute] =
      new mutable.PriorityQueue[ChitonRoute]()(Ordering.by(-_.cost)) += defaultRoute

    ChitonPath(matrix, priorityQueue, Map.empty)
  }

  /**'
   * Below is the brute force answer. I'm keeping it here because of completeness, but I do want to
   * remove the code.
   *
   * // Djikstra's algorithm finds shortest path to all nodes, where we care about
   * shortest path to a single node.
   * 1. If at start node we have visitable neighbors so we add ourselves to the visited set.
   * 2. We move forward at the cheapest nearest neighbor.
   * 3. If we are at our neighbor and we need to backtrack, we need to add ourselves to the visited
   *    set and remove nearestneighbors not in our current path.
   *
   * We are going to do a depth first
   * @param chitonPath
   * @return
  @tailrec
  def findLeastRiskyPath(chitonPath: ChitonPath): ChitonPath = {

    if (chitonPath.routes.isEmpty || chitonPath.isCurrentRouteLongerThanShortest) {
      chitonPath.routes.clear()
      chitonPath
    } else if (chitonPath.isAtEndNode) {
      val shortestRoute = endUpdateShortestRoute(chitonPath)
      val tailRoutes = chitonPath.routes.tail

      findLeastRiskyPath(chitonPath.copy(routes = tailRoutes, shortestRoute = shortestRoute))
    } else {
      val updatedRoutes = routesAddingCurrentNeighbors(chitonPath)

      findLeastRiskyPath(chitonPath.copy(routes = updatedRoutes))
    }
  }

  private[model] def routesAddingCurrentNeighbors(chitonPath: ChitonPath):
  mutable.PriorityQueue[ChitonRoute] = {
    val currentRoute = chitonPath.currentRoute

    val routesWithNeighbors: List[ChitonRoute] = chitonPath
      .currentVisitableNeighbors
      .map{neighbor =>
        val updatedPath = currentRoute.path + neighbor.location
        val updatedCost = currentRoute.cost + neighbor.value

        ChitonRoute(updatedPath, updatedCost, neighbor.location)
      }

    chitonPath.routes.tail ++ routesWithNeighbors
  }

  private[model] def endUpdateShortestRoute(chitonPath: ChitonPath):
  Option[ChitonRoute] = {
    chitonPath
      .shortestRoute
      .map{ route =>
        if (chitonPath.currentPathDistance <  route.cost)
          chitonPath.currentRoute
        else
          route
      }.orElse(Some(chitonPath.currentRoute))
  }
 **/
}
