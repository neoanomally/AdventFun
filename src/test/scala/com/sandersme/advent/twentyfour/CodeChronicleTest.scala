package com.sandersme.advent.twentyfour

class CodeChronicleTest extends munit.FunSuite {

  test("Test to make sure that the keys and locks are parsed correctly, 2 keys 3 locks") {
    assertEquals(CODE_CHRONICLE.keys.size, 3)
    assertEquals(CODE_CHRONICLE.locks.size, 2)
    

    assertEquals(CODE_CHRONICLE.locks.head, Vector(0, 5, 3, 4, 3))
    assertEquals(CODE_CHRONICLE.locks(1), Vector(1, 2, 0, 5, 3))

    assertEquals(CODE_CHRONICLE.keys.head, Vector(5,0,2,1,3))
    assertEquals(CODE_CHRONICLE.keys.last, Vector(3,0,2,0,1))
  }

  test("Count the number of key-lock combinations") {
    val combinations = CODE_CHRONICLE.countNumKeyLockFits


    assertEquals(combinations, 3)
  }

  val TEST_INPUT = """#####
                      .####
                      .####
                      .####
                      .#.#.
                      .#...
                      .....

                      #####
                      ##.##
                      .#.##
                      ...##
                      ...#.
                      ...#.
                      .....

                      .....
                      #....
                      #....
                      #...#
                      #.#.#
                      #.###
                      #####

                      .....
                      .....
                      #.#..
                      ###..
                      ###.#
                      ###.#
                      #####

                      .....
                      .....
                      .....
                      #....
                      #.#..
                      #.#.#
                      #####""".stripMargin.split("\n").toList.map(_.trim)

  val CODE_CHRONICLE = CodeChronicle.parseInput(TEST_INPUT)
}
