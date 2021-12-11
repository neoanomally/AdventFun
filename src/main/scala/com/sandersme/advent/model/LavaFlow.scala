package com.sandersme.advent.model

import LavaFlow.{LavaNode, ValueNeighbors}

case class LavaFlow(flows: List[List[LavaNode]])  {

//  def findAllNeighorsEachPoint: List[List[ValueNeighbors]] = {
//    (flows.indices).map{ y =>
//      flows(0).indices.map{ x =>
//        val xVals = List(x + 1, x - 1).filter(_ >= 0).filter(_ < flows(y).length)
//        val yVals = List(y + 1, y - 1).filter(_ >= 0).filter(_ < flows.length)
//
//        val value = flows(y)(x)
//        val neighbors = xVals.map(xV => flows(y)(xV)) ++
//          yVals.map(yV => flows(yV)(x))
//
//        ValueNeighbors(value, neighbors)
//      }.toList
//    }.toList
//  }

  def findLowestPeaks: List[LavaNode] = {
    flows.flatten
    .filter(lavaNode => lavaNode
      .neighbors
      .forall(neighbor => neighbor.value > lavaNode.value)
    )
  }

  def calculateSumRisk: Int = {
    findLowestPeaks.map(_.value + 1).sum
  }
}
object LavaFlow {
  case class LavaNode(value: Int, neighbors: List[LavaNode])

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

        def currValue = flows(y)(x)
        def neighbors: List[LavaNode] = xVals.map(xV => outputArray(y)(xV)) ++
          yVals.map(yV => outputArray(yV)(x))

        var node = LavaNode(currValue, neighbors)
        outputArray(y)(x) = node
      }
    }

    LavaFlow(outputArray.map(_.toList).toList)
  }
}
