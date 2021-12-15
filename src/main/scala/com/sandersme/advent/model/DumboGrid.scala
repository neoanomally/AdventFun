package com.sandersme.advent.model

import com.sandersme.advent.model.DumboGrid.Node

import scala.annotation.tailrec


case class DumboGrid(nodes: List[List[Node]], haveAllFlashed: Boolean = false,
                     stepAllFlashed: Int = 0) {
  def totalNumberFlashes: Long = nodes.flatMap(_.map(_.numFlashes)).sum
}

object DumboGrid {
  // TODO: We need to update whether or not something flashed, and reset it after it's
  // flashed. That way we keep flashing as octopus energy increments
  case class Node(value: Int, neighbors: List[Point], flashing: Boolean = false,
                  numFlashes: Long = 0, alreadyFlashed: Boolean = false)

  def energyIncrease(dumboGrid: DumboGrid): DumboGrid = {
    val nodesAfterEnergyIncrease = increaseEnergyAllNodes(dumboGrid.nodes)
    val nodesAfterExploding = explodeAllNodes(nodesAfterEnergyIncrease)
    val nodesAfterReleasedAllEnergy = releaseEnergy(nodesAfterExploding)
    val haveAllFlashed = nodesAfterExploding.forall(_.forall(_.alreadyFlashed))

    DumboGrid(nodesAfterReleasedAllEnergy, haveAllFlashed)
  }

  @tailrec
  def findStepAllOctopusFlash(dumboGrid: DumboGrid, step: Int = 0, max: Int = 500): DumboGrid = {
    val hasReachedMaxStep = step >= max
    if (dumboGrid.haveAllFlashed || hasReachedMaxStep) {
      dumboGrid.copy(stepAllFlashed = step)
    } else {
      val dumboGridIncreasedEnergy = energyIncrease(dumboGrid)
      findStepAllOctopusFlash(dumboGridIncreasedEnergy, step + 1)
    }
  }

  private[model] def increaseEnergyAllNodes(nodes: List[List[Node]]): List[List[Node]] = {
    nodes
      .map(_.map(node => updateNode(node)))
  }

  // TODO: This is going to be a recursive loop. We will need to use the flashed
  // variable here. One of the things that's going to slow us down is that
  // we are going to have to loop through each time to see if there was a single
  // flash... We might be able to do this with a var, but then we won't be pure. \
  @tailrec
  private[model] def explodeAllNodes(nodes: List[List[Node]]): List[List[Node]] = {
    val updatedNodes = nodes.map(_.map { node =>
      if (node.flashing || node.alreadyFlashed) {
        node.copy(flashing = false, alreadyFlashed = true)
      } else {
        val numberOfFlashingNeighbors = countFlashedNeighbors(node, nodes)
        updateNode(node, numberOfFlashingNeighbors)
      }
    })

    // If any node has flashed we need to explodeAllNodes Again
    if (updatedNodes.exists(_.exists(_.flashing)))
      explodeAllNodes(updatedNodes)
    else
      updatedNodes
  }

  /**
   * 1. Update the node value by one
   * 2. Flash only once per step so it should only flash at value 9, but can still increment
   *
   * @param node
   * @return
   */
  private[model] def updateNode(node: Node, increment: Int = 1): Node = {
    val updatedValue = node.value + increment
    val flashing = updatedValue > 9
    val numFlashes = if (flashing) node.numFlashes + 1 else node.numFlashes

    node.copy(value = updatedValue, numFlashes = numFlashes, flashing = flashing)
  }

  // TODO add test for reseting all exploded nodes value > 9
  private[model] def releaseEnergy(nodes: List[List[Node]]): List[List[Node]] = {
    nodes.map(_.map{ node =>
      if (node.value > 9)
        node.copy(value = 0, flashing = false, alreadyFlashed = false)
      else
        node
    })
  }

  private[model] def countFlashedNeighbors(node: Node,
                                           nodes: List[List[Node]]): Int = {

    if (!node.flashing || !node.alreadyFlashed) {
      findAllNeighbors(nodes, node.neighbors).count(_.flashing)
    } else {
      0
    }
  }

  private[model] def hasAnyNeighborFlashed(nodes: List[List[Node]],
                                           points: List[Point]): Boolean = {
    findAllNeighbors(nodes, points).exists(_.flashing)
  }

  private[model] def findAllNeighbors(nodes: List[List[Node]],
                                      points: List[Point]): List[Node] = {
    points.map(point => nodes(point.y)(point.x))
  }


  def parseInput(input: List[String]): DumboGrid = {
    val inputValues: List[List[Int]] = input
      .map(_.toCharArray.map(_.toString.toInt).toList)

    val maxX = inputValues.length
    val maxY = inputValues.head.length

    val nodes = inputValues.zipWithIndex.map{ case(rows, y) =>
      rows.zipWithIndex.map{ case(value, x) =>
        val neighbors: List[Point] = generateNeighborValues(x, y, maxX, maxY)

        Node(value, neighbors)
      }
    }

    DumboGrid(nodes)
  }

  private[model] def generateNeighborValues(x: Int, y: Int, maxX: Int, maxY: Int): List[Point] = {
    val results = for {
      xVal <- (-1 to 1).map(_ + x)
      yVal <- (-1 to 1).map(_ + y)

      if(xVal >= 0 && yVal >= 0) &&
        (xVal < maxX && yVal < maxY) &&
        (xVal != x || y != yVal)
    }  yield Point(xVal, yVal)

    results.toList
  }
}
