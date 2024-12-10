package com.sandersme.advent.twentyfour

class HoofItTest extends munit.FunSuite {
  test("Validate that I can do something to the grid") {
    assertEquals(HoofIt.parseInput(TEST_INPUT).traverseGrid, 36)
  }

  test("Validate that I can traverse the grid for part 2") {
    assertEquals(HoofIt.parseInput(TEST_INPUT).traverseGrid2, 81)
  }

  val TEST_INPUT = """89010123
                      78121874
                      87430965
                      96549874
                      45678903
                      32019012
                      01329801
                      10456732""".stripMargin
                    .split("\n")
  .map(_.trim)
  .toList


  val TEST_INPUT_TWO = """...0...
...1...
...2...
6543456
7.....7
8.....8
9.....9"""
  .stripMargin
  .split("\n")
  .map(_.trim)
  .toList

  val TEST_GRID = HoofIt.parseInput(TEST_INPUT)
}
