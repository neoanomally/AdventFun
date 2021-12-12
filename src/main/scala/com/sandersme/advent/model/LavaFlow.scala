package com.sandersme.advent.model

import LavaFlow.{Basin, LavaNode, ValueNeighbors, Point}

case class LavaFlow(flows: List[List[LavaNode]])  {

  def findAllBasins: List[Basin] = {
    // validated that this will fill everything false
    val visitedNodes = Array.ofDim[Boolean](flows.length, flows.head.length)

    val basins: List[Basin] = flows.indices.flatMap { y =>
      flows(y).indices.flatMap { x =>
        val hasNotBeenVisited = !visitedNodes(y)(x)

        if (hasNotBeenVisited) {
          val basin: Basin = Basin(visitAllNeighbors(Point(x, y), visitedNodes))
          Some(basin)
        } else {
          None
        }
      }
    }.toList

    basins.filter(_.values.nonEmpty)
  }

  def calculateProductTopThreeBasins: Int = {
    findAllBasins
      .map(_.values.size)
      .sorted(Ordering[Int].reverse)
      .take(3)
      .product
  }

  private def visitAllNeighbors(currentNode: Point,
                                visitedNodes: Array[Array[Boolean]]): List[Int] = {
    val currentFlow = flows(currentNode.y)(currentNode.x)
    val hasNotBeenVisited = !visitedNodes(currentNode.y)(currentNode.x)
    val isNotPeak = currentFlow.value != 9
    visitedNodes(currentNode.y)(currentNode.x) = true

    if(hasNotBeenVisited && isNotPeak) {
      visitedNodes(currentNode.y)(currentNode.x) = true

      currentFlow
        .neighbors
        .filter(point => !visitedNodes(point.y)(point.x))
        .filter(point => flows(point.y)(point.x).value != 9)
        .flatMap( point =>
          visitAllNeighbors(point, visitedNodes)
        ) :+ currentFlow.value

    }  else {
      List.empty
    }
  }

  def findLowestPeaks: List[LavaNode] = {
    flows.flatten
    .filter(lavaNode => lavaNode
      .neighbors
      .forall{ neighbor =>
        flows(neighbor.y)(neighbor.x).value > lavaNode.value
      }
    )
  }

  def calculateSumRisk: Int = {
    findLowestPeaks.map(_.value + 1).sum
  }
}


object LavaFlow {
  case class Basin(values: List[Int])
  case class LavaNode(value: Int, neighbors: List[Point])
  case class Point(x: Int, y: Int)
  case class ValueNeighbors(value: Int, neighbors: List[Int])



  // MUTATIONS OH MY BUT WE NEED TO POPULATE THIS THING
  def parseInput(input: Array[String]): LavaFlow = {
    val flows = input.map(_.map(_.toString.toInt).toArray)
    val outputArray: Array[Array[LavaNode]] =
      Array.ofDim[LavaNode](flows.length, flows(0).length)

    flows.indices.foreach{ y =>
      flows(0).indices.foreach{ x =>
        val xVals = List(x + 1, x - 1).filter(_ >= 0).filter(_ < flows(y).length)
        val yVals = List(y + 1, y - 1).filter(_ >= 0).filter(_ < flows.length)

        val currValue = flows(y)(x)
        val neighbors: List[Point] = xVals.map(xV => Point(xV, y)) ++
          yVals.map(yV => Point(x, yV))

        outputArray(y)(x) = LavaNode(currValue, neighbors)
      }
    }

    LavaFlow(outputArray.map(_.toList).toList)
  }
}
