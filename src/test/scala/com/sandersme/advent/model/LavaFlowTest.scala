package com.sandersme.advent.model

import com.sandersme.advent.model.LavaFlow.LavaNode

class LavaFlowTest extends munit.FunSuite {
  val TEST_INPUT: Array[String] = Array("2199943210", "3987894921",
    "9856789892", "8767896789", "9899965678")

  val PARSED_LAVA_FLOW: LavaFlow = LavaFlow.parseInput(TEST_INPUT)


  test("Lava flow parseInput returns back a lava flow object") {
    val testInput: Array[String] = Array("1039209503509305934", "1039209503509306789")
    val lavaFlow: LavaFlow = LavaFlow.parseInput(testInput)
    println(lavaFlow.flows)
    assertEquals(lavaFlow.flows.length, 2)
  }

  test("Find all neighbors with minimum peak") {
    val lavaFlow: LavaFlow = PARSED_LAVA_FLOW
    val sumRisk = lavaFlow.calculateSumRisk

    val expectedRisk = 15
    assertEquals(sumRisk, expectedRisk)

  }

  test("Find all the basins within an area") {
    ???
  }

}
