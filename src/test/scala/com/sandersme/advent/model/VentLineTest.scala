package com.sandersme.advent.model

import com.sandersme.advent.graph.Point
import junit.framework.Assert.assertEquals
import junit.framework.TestCase.assertFalse
import junit.framework.TestCase.assertTrue

class VentLineTest extends munit.FunSuite {
  val TEST_INPUT = """0,9 -> 5,9
                     |8,0 -> 0,8
                     |9,4 -> 3,4
                     |2,2 -> 2,1
                     |7,0 -> 7,4
                     |6,4 -> 2,0
                     |0,9 -> 2,9
                     |3,4 -> 1,4
                     |0,0 -> 8,8
                     |5,5 -> 8,2""".stripMargin
    .linesIterator.toList

  test("Parse Vent lines 9,5 -> 5, 9") {
    val input = "9,5 -> 5,9"
    val expectedVentLine = VentLine(Point(9, 5), Point(5, 9))

    val parsedVentLine = VentLine.parseInput(input)
    assertEquals(parsedVentLine, expectedVentLine)
  }

  test("Parse Multiple Lines of Vent inputs. The last one should equal 5,5 -> 8,2") {
    val parsedVentLines = VentLine.parseInputs(TEST_INPUT)
    val expectedLastValue = VentLine(Point(5, 5), Point(8, 2))

    assertEquals(parsedVentLines.last, expectedLastValue)
  }


  test("Create a line which is a list of points from a vent line") {
    val ventLine = VentLine(Point(7, 0), Point(7, 4))
    val expectedOutput = List(Point(7, 0), Point(7, 1), Point(7, 2), Point(7, 3), Point(7, 4))
    val generatedPoints  = ventLine.generateAllLinePoints

    assertEquals(generatedPoints, expectedOutput)
  }

  test("We should only draw lines with overlapping x & y axis. Testing result should be empty") {
    val ventLine = VentLine(Point(1, 2), Point(4, 8))

    val linePoints: List[Point] = ventLine.generateAllLinePoints
    val expectedEmpty: List[Point] = List.empty

    assertEquals(linePoints, expectedEmpty)
  }

  test("9,5 and 5,9 are diagnal to one another") {
    val ventLine =  VentLine(Point(9, 5), Point(5, 9))
    val isDiagonal = ventLine.isDiagonalLine

    assertTrue(isDiagonal)
  }

  test("9,5 and 6, 1 are not diagnal to one another") {
    val ventLine =  VentLine(Point(9, 5), Point(6, 1))
    val isDiagonal = ventLine.isDiagonalLine

    assertFalse(isDiagonal)
  }

  test("For each Ventline Calculate all overlapping points without diagonal line") {
    val ventLines: List[VentLine] = VentLine.parseInputs(TEST_INPUT)
    val expectedNumberOfOverlappingPoints = 5

    val numberOfOverlappingPoints = VentLine
      .countNumberOfOverlappingPoints(ventLines,ventline =>
        ventline.generateAllVerticalAndHorizontalPoints)

    assertEquals(numberOfOverlappingPoints, expectedNumberOfOverlappingPoints)
  }

  test("For each Ventline Calculate all overlapping points with diagonal line") {
    val ventLines: List[VentLine] = VentLine.parseInputs(TEST_INPUT)
    val expectedNumberOfOverlappingPoints = 12

    val numberOfOverlappingPoints = VentLine
      .countNumberOfOverlappingPoints(ventLines)

    assertEquals(numberOfOverlappingPoints, expectedNumberOfOverlappingPoints)
  }

  test("Should generate points for VentLine(Point(0, 9), Point(5, 9))") {
    val ventLine = VentLine(Point(0, 9), Point(5, 9))
    val ventLineB = VentLine(Point(5, 9), Point(0, 9))
    val generatedPoints = ventLine.generateAllLinePoints
    val generatedPointsB = ventLineB.generateAllLinePoints.reverse

    assertEquals(generatedPoints, generatedPointsB)
  }


  test("Test generating range 1 to 5 and 5 to 1 should equal the same") {
    val a = VentLine.generateRange(1, 5)
    val reversedB = VentLine.generateRange(5, 1).reverse

    val expectedSize = 5

    assertEquals(a, reversedB)
    assertEquals(a.size, expectedSize)
  }
}
