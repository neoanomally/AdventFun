package com.sandersme.advent.twentyfour

class GardenGroupsTest extends munit.FunSuite {
  test("Starting a test now") {
    val findAllGroups = GardenGroups.findAllConnectedPoints(PARSED_INPUT)

    println(findAllGroups)
  } 

  test("Calculate value should equal to 1930") {
    val findAllGroups = GardenGroups.findAllConnectedPoints(PARSED_INPUT)
    
    assertEquals(findAllGroups.calculatePlots, 1930)
  }

  test("Validate that we can correctly calculate part 2") {
    val findAllGroups = GardenGroups.findAllConnectedPoints(PARSED_INPUT)
    assertEquals(findAllGroups.calculatePlotsPart2, 1206)
  }

  val TEST_INPUT = """RRRRIICCFF
      RRRRIICCCF
      VVRRRCCFFF
      VVRCCCJFFF
      VVVVCJJCFE
      VVIVCCJJEE
      VVIIICJJEE
      MIIIIIJJEE
      MIIISIJEEE
      MMMISSJEEE""".stripMargin
    .split("\n")
    .toList
    .map(_.trim)

  val PARSED_INPUT = GardenGroups.parseInput(TEST_INPUT)
}
