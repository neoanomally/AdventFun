package com.sandersme.advent.twentyone.model

import DumboGrid.{DumboNode, increaseEnergyAllNodes, releaseEnergy}
import com.sandersme.advent.twentyone.graph.Point

class DumboGridTest extends munit.FunSuite {
  val TEST_INPUT = List(   "5483143223",
                           "2745854711",
                           "5264556173",
                           "6141336146",
                           "6357385478",
                           "4167524645",
                           "2176841721",
                           "6882881134",
                           "4846848554",
                           "5283751526")

  val TEST_DUMBO_GRID = DumboGrid.parseInput(TEST_INPUT)

  test("Validate We can read the data") {
    val dumboGrid = DumboGrid.parseInput(TEST_INPUT)

    val maxValue = dumboGrid.nodes.flatMap(_.map(_.value)).max

    val hasOutOfBoundsNeighbor = TEST_DUMBO_GRID.nodes.exists(_.exists(node =>
      node.neighbors.contains(Point(10, 0))))

    assertEquals(maxValue, 8)
    assertEquals(hasOutOfBoundsNeighbor, false)
  }

  test("Need to cause the octopus to flash") {
    val TINY_NODE_TEST = DumboGrid.parseInput(List("11111","19991", "19191", "19991", "11111"))

    val increaseEnergyStep1 = DumboGrid.energyIncrease(TINY_NODE_TEST)
    val totalNumFlashes = increaseEnergyStep1.nodes.flatMap(_.map(_.numFlashes)).sum

    assertEquals(totalNumFlashes, 9L)
    assertEquals(increaseEnergyStep1.nodes.head.head.value, 3)
    assertEquals(increaseEnergyStep1.nodes(1).head.value, 4)
    assertEquals(increaseEnergyStep1.nodes(2).head.value, 5)

  }

  test("Increasing Nodes 100 Steps") {
    val expected100StepOutput = DumboGrid.parseInput(
      List( "0397666866",
            "0749766918",
            "0053976933",
            "0004297822",
            "0004229892",
            "0053222877",
            "0532222966",
            "9322228966",
            "7922286866",
            "6789998766"
          ))

    val dumboGrid100Steps = (1 to 100).foldLeft(TEST_DUMBO_GRID){case (dumboGrid, step) =>
      DumboGrid.energyIncrease(dumboGrid)
    }

    val expectedEndValues = expected100StepOutput.nodes.map(_.map(_.value))
    val endValues = dumboGrid100Steps.nodes.map(_.map(_.value))
    val endNumFlashes = dumboGrid100Steps.totalNumberFlashes
    val expectedEndNumFlashes = 1656L

    assertEquals(endValues, expectedEndValues)
    assertEquals(endNumFlashes, expectedEndNumFlashes)
  }

  test("Calculate the step when ALL Octopus flash") {
    val dumboGrid194Steps = (1 to 194)
      .foldLeft(TEST_DUMBO_GRID){ case(dumboGrid, _) =>
        DumboGrid.energyIncrease(dumboGrid)
    }

    val dumboGrid195Steps = DumboGrid.energyIncrease(dumboGrid194Steps)

    assertEquals(dumboGrid194Steps.haveAllFlashed, false)
    assertEquals(dumboGrid195Steps.haveAllFlashed, true)
  }

  test("Tail recursion till we find max step and step number") {
    val stepAllOctopusFlash = DumboGrid.findStepAllOctopusFlash(TEST_DUMBO_GRID)

    assertEquals(stepAllOctopusFlash.stepAllFlashed, 195)
  }

  test("Test find all neighbors") {
    val neighborsZeroZero = TEST_DUMBO_GRID.nodes.head.head
    val hasAnyNodeFlashed = DumboGrid
      .hasAnyNeighborFlashed(TEST_DUMBO_GRID.nodes, neighborsZeroZero.neighbors)

    val expectedNoNodesFlashed = false
    val expectedNodesFlashed = true

    assertEquals(hasAnyNodeFlashed, expectedNoNodesFlashed)
  }

  test("Increase ALl Energy Nodes") {
    val nodesWithIncreasedEnergy = DumboGrid.increaseEnergyAllNodes(TEST_DUMBO_GRID.nodes)

    val node3x0y = nodesWithIncreasedEnergy.head(2)
    val expectedNode3 = DumboNode(9,
      List(Point(1, 0), Point(1, 1), Point(2, 1), Point(3, 0), Point(3, 1)), false, 0, false)

    val node3x1y = nodesWithIncreasedEnergy(1)(2)
    val expected3x1y = DumboNode(5, List(Point(1, 0), Point(1, 1), Point(1, 2), Point(2, 0),
      Point(2, 2), Point(3, 0), Point(3, 1), Point(3, 2)), false, 0)

    assertEquals(node3x0y, expectedNode3)
    assertEquals(node3x1y, expected3x1y)
  }

  test("Release energy for all nodes") {
    val mockedNodes = TEST_DUMBO_GRID.nodes.map(_.map{node =>
      if(node.value > 5) node.copy(value = 10) else node
    }) // first increase every node to ten

    val numberOfNodesBeforeReleaseAbove5 = mockedNodes.map(_.count(_.value == 10)).sum
    val releasedEnergyNodes  = releaseEnergy(mockedNodes)
    val numberOfNodesAfterReleaseAbove5 = releasedEnergyNodes.map(_.count(_.value == 10)).sum
    val numberOfNodesAfterReleasedEnergyAbove0 = releasedEnergyNodes
      .map(_.count(_.value > 0)).sum

    assertEquals(numberOfNodesAfterReleaseAbove5, 0)
    assert(numberOfNodesBeforeReleaseAbove5 > numberOfNodesAfterReleaseAbove5)
    assertEquals(numberOfNodesAfterReleasedEnergyAbove0, 66)
  }

  test("Update node should increment node and any values") {
    val testNode = DumboNode(8, List.empty)
    val updatedNode = DumboGrid.updateNode(testNode)
    val expectedNode = DumboNode(9, List.empty, false, 0)

    val updatedTwiceNode = DumboGrid.updateNode(updatedNode)
    val expectedUpdatedTwiceNode = DumboNode(10, List.empty, true, 1)

    assertEquals(updatedNode, expectedNode)
    assertEquals(updatedTwiceNode, expectedUpdatedTwiceNode)
  }
}
