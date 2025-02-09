package com.sandersme.advent.twentythree

class ParabolicReflectorDishTest extends munit.FunSuite {
  test("Move all the rocks northern most point") {
    TEST_DISH.tiltRocksNorth
  }

  test("Calculate load of rocks after tilt") {
    val load = TEST_DISH.tiltAndRotate 
    assertEquals(load, 64)
  }

  test("Calculate load of rocks after one tilt north") {
    val load = TEST_DISH.tiltRocksNorth.calculateLoad 
    assertEquals(load, 136)
  }
  
  test("Spin 3 times printing them each".ignore) {
    TEST_DISH.spinNCyclesAndPrint(3)
  }



  val TEST_INPUT = """  O....#....
                        O.OO#....#
                        .....##...
                        OO.#O....O
                        .O.....O#.
                        O.#..O.#.#
                        ..O..#O..O
                        .......O..
                        #....###..
                        #OO..#....""".split("\n").toList.map(_.trim)
  val TEST_DISH = ParabolicReflectorDish.parseInput(TEST_INPUT)
}
