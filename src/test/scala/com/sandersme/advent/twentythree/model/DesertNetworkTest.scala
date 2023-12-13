package com.sandersme.advent.twentythree.model

import scala.annotation.experimental


class DesertNetworkTest extends munit.FunSuite {
  val TEST_INPUT = 
    """RL

      |AAA = (BBB, CCC)
      |BBB = (DDD, EEE)
      |CCC = (ZZZ, GGG)
      |DDD = (DDD, DDD)
      |EEE = (EEE, EEE)
      |GGG = (GGG, GGG)
      |ZZZ = (ZZZ, ZZZ)"""
        .stripMargin
        .split("\n")
        .toList

  val TEST_GHOST_INPUT = 
    """LR

      |11A = (11B, XXX)
      |11B = (XXX, 11Z)
      |11Z = (11B, XXX)
      |22A = (22B, XXX)
      |22B = (22C, 22C)
      |22C = (22Z, 22Z)
      |22Z = (22B, 22B)
      |XXX = (XXX, XXX)""".stripMargin
        .split("\n")
        .toList
  val DESERT_NETWORK = DesertNetwork.parseInput(TEST_INPUT)
  val GHOST_NETWORK = DesertNetwork.parseInput(TEST_GHOST_INPUT)

  test("Validate that we can parse into our Desert Network Graph") {
    val directions: List[Direction] = DESERT_NETWORK.directions.values
    val expectedDirections: List[Direction] = List(Direction.Right, Direction.Left)

    assertEquals(directions, expectedDirections)
  }

  test("Validate that we can update the directions") {

    val initialPeek = DESERT_NETWORK.directions.peek
    val expectedInitialPeek = Direction.Right
    val updatedDirections = DESERT_NETWORK.directions.moveOneDirection

    val nextStep: Direction = updatedDirections.peek
    val expectedNextStep = Direction.Left 

    assertEquals(initialPeek, expectedInitialPeek)
    assertEquals(nextStep, expectedNextStep)
  }

  test("Validate that the graph is parsed Correctly") {
    val expectedGraphVectors = Vector(
      DesertNode("AAA", "BBB", "CCC"),
      DesertNode("BBB", "DDD", "EEE"),
      DesertNode("CCC", "ZZZ", "GGG"),
      DesertNode("DDD", "DDD", "DDD"),
      DesertNode("EEE", "EEE", "EEE"),
      DesertNode("GGG", "GGG", "GGG"),
      DesertNode("ZZZ", "ZZZ", "ZZZ")
    )

    val graph = DESERT_NETWORK.graph.nodes

    assertEquals(graph, expectedGraphVectors)
  }

  test("Find prime factors of the number 6 should equal 2 and 3") {
    val primeFactors = DesertNetwork.findPrimeFactors(6)
    val expectedValues = List(2, 3)

    assertEquals(primeFactors, expectedValues)
  }

  test("Validate that we can get a specific node within the graph") {
    val node = DESERT_NETWORK.graph.findNode("DDD")
    val expectedNode = DesertNode("DDD", "DDD", "DDD")
    assertEquals(node, expectedNode)
  }

  test("validate that we can traverse the test graph in two steps") {
    val expectedSteps = 2
    val numStepsToExit = DESERT_NETWORK.findStepsToExit

    assertEquals(numStepsToExit, 2)
  }

  test("validate that we can traverse the ghost network in six steps") {
    val expectedResult = 6L
    val cyclesFrorNode = GHOST_NETWORK.findCyclesAndExitPoints
    println(s"CYCLES: ${cyclesFrorNode}")
    val results = GHOST_NETWORK.calculateStepsFromCycles

    assertEquals(results, expectedResult)
  }
}
