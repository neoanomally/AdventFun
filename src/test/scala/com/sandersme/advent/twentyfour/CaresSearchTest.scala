package com.sandersme.advent.twentyfour

class CaresSearchTest extends munit.FunSuite {
  test("Test the width should equal 10") {
    assertEquals(TEST_GRID.width, 10)
  }

  test("Test the height should equal 10") {
    assertEquals(TEST_GRID.height, 11)
  }


  test("Test Valid locations") {
    val outOfBoundsOne = TEST_GRID.validLoc(Point(-1, 0))
    val outOfBoundsTwo = TEST_GRID.validLoc(Point(12, 0))
    val outOfBoundsThree = TEST_GRID.validLoc(Point(9, 10))
    val validLocOne = TEST_GRID.validLoc(Point(0, 0))
    val validLocTwo = TEST_GRID.validLoc(Point(10, 9))


    assertEquals(outOfBoundsOne, false)
    assertEquals(outOfBoundsTwo, false)
    assertEquals(outOfBoundsThree, false)
    assertEquals(validLocOne, true)
    assertEquals(validLocTwo, true)
  }


  test("Test findAllNeighborCounts should return nothing") {
    val shouldEqualOne = TEST_GRID.countStraightLines(Point(0, 5), "XMAS")
    val shouldEqualZero = TEST_GRID.countStraightLines(Point(0, 0), "XMAS")
    
    assertEquals(shouldEqualZero, 0)
    assertEquals(shouldEqualOne, 1)
  }


  test("Test count all occurrences") {
    val expectedEighteen = TEST_GRID.countAllStraightLineOccurrences("XMAS")
    assertEquals(expectedEighteen, 18)
  }

  test("Test that the characters match up on looking up charAt") {
    val X = TEST_GRID.charAt(Point(0, 5)) 
    val M = TEST_GRID.charAt(Point(0, 6))

    assertEquals(X, 'X')
    assertEquals(M, 'M')
  }


 /// ADDED AN Additional row to check some height differences
  val TEST_INPUT: List[String] = """MMMSXXMASM
      MSAMXMSMSA
      AMXSXMAAMM
      MSAMASMSMX
      XMASAMXAMM
      XXAMMXXAMA
      SMSMSASXSS
      SAXAMASAAA
      MAMMMXMMMM
      MXMXAXMASX
      ..........""".stripMargin
  .split("\n")
  .toList
  .map(_.trim)
  

  val TEST_GRID = CareGrid.create(TEST_INPUT)
  
}
