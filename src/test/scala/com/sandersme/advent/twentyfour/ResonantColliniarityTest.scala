package com.sandersme.advent.twentyfour

import com.sandersme.advent.twentyfour.ResonantCollinearity.calculateAntiNodes
import com.sandersme.advent.twentyfour.ResonantCollinearity.calculateRiseRun



class ResonantColliniarityTest extends munit.FunSuite {

  test("Calulcate the slope of different points") {
    val pointOne = ResonantPoint(6, 5)
    val pointOneTemp = ResonantPoint(6, 6)
    val pointTwo= ResonantPoint(8, 8)
    val pointThree = ResonantPoint(9, 9)
    val pointFour  = ResonantPoint(1, 9)
    val pointFive = ResonantPoint(8, 4)

    val slopeOneTwo = ResonantCollinearity.calculateSlope(pointOne, pointTwo)
    val slopeTwoThree = ResonantCollinearity.calculateSlope(pointTwo, pointThree)
    val slopeOneThree = ResonantCollinearity.calculateSlope(pointOne, pointThree)
    val slopeOneTempThree = ResonantCollinearity.calculateSlope(pointOneTemp, pointThree)
    val slopeFourThree = ResonantCollinearity.calculateSlope(pointThree, pointFour)
    val slopeOneTempFive= ResonantCollinearity.calculateSlope(pointOneTemp, pointFive)
    val slopeFiveOneTemp = ResonantCollinearity.calculateSlope(pointFive, pointOneTemp)

    assertEquals(slopeOneTwo, 1.5)
    assertEquals(slopeTwoThree, 1.0)
    assert(Math.abs(slopeOneThree - 1.333333) < .01)
    assertEquals(slopeTwoThree, 1.0)
    assertEquals(slopeFourThree, 0.0)
    assertEquals(slopeOneTempFive, -1.0)
    assertEquals(slopeFiveOneTemp, - 1.0)
  }

  test("Calculate points that are in a straight line") {
    val pointTwo= ResonantPoint(8, 8)
    val pointThree = ResonantPoint(9, 9)
    val pointFour  = ResonantPoint(1, 9)

    val straightLine = ResonantCollinearity.isStraightLine(pointTwo, pointThree)
    val horizontalLine = ResonantCollinearity.isStraightLine(pointFour, pointThree)
    val notStraightLine = ResonantCollinearity.isStraightLine(pointTwo, pointFour)

    assertEquals(straightLine, true)
    assertEquals(horizontalLine, true)
    assertEquals(notStraightLine, false)
  }

  test("Calculate slope type") {
    val pointTwo= ResonantPoint(8, 8)
    val pointThree = ResonantPoint(9, 9)
    val pointFour  = ResonantPoint(1, 9)
    val pointOne = ResonantPoint(9, 7)
    val pointFive = ResonantPoint(8, 6)

    val (negRise, negRun) = calculateRiseRun(pointTwo, pointThree)
    val (horizontalRize, horizontalRun) = calculateRiseRun(pointThree, pointFour)
    val (posRise, posRun) = calculateRiseRun(pointOne, pointTwo)
    val (verticalRise, verticalRun) = calculateRiseRun(pointTwo, pointFive)

    val slopeNeg = ResonantCollinearity.calcSlopeType(negRise, negRun)
    val slopeHorizontal = ResonantCollinearity.calcSlopeType(horizontalRize, horizontalRun)
    val slopePos = ResonantCollinearity.calcSlopeType(posRise, posRun)
    val slopeVertical = ResonantCollinearity.calcSlopeType(verticalRise, verticalRun)

    assertEquals(slopePos, PosDiagnal)
    assertEquals(slopeHorizontal, Horizontal)
    assertEquals(slopeNeg, NegDiagnal)
    assertEquals(slopeVertical, Vertical)
  }

  test("Calculate AntiNodes Around two points in a line") {
    val pointA = ResonantPoint(8, 8)
    val pointB = ResonantPoint(9, 9)

    val calculations = ResonantCollinearity.calculateAntiNodes(pointA, pointB)
    val calculationsBackwards = ResonantCollinearity.calculateAntiNodes(pointB, pointA)
    val expected = List(ResonantPoint(7, 7), ResonantPoint(10, 10))

    assertEquals(calculations, expected)
    assertEquals(calculationsBackwards, expected)

    val pointC = ResonantPoint(6, 4)
    val pointD = ResonantPoint(4, 4)
    val expectedCD = List(ResonantPoint(2, 4), ResonantPoint(8, 4))

    assertEquals(ResonantCollinearity.calculateAntiNodes(pointC, pointD), expectedCD)
    
    val pointE = ResonantPoint(6, 4)
    val pointF = ResonantPoint(4, 2)
    val expectedEF = List(ResonantPoint(2, 0), ResonantPoint(8, 6))

    assertEquals(ResonantCollinearity.calculateAntiNodes(pointE, pointF), expectedEF)

    val pointG = ResonantPoint(6, 4)
    val pointH = ResonantPoint(7, 5)
    val expectedGH = List(ResonantPoint(5, 3), ResonantPoint(8, 6))

    assertEquals(ResonantCollinearity.calculateAntiNodes(pointG, pointH), expectedGH)
  }

  test("Calculate the distance between two points") {

    val pointTwo= ResonantPoint(8, 8)
    val pointThree = ResonantPoint(9, 9)
    val pointFour  = ResonantPoint(1, 9)

    val distanceTwoThree = ResonantCollinearity.calculateDistance(pointTwo, pointThree)
    val distanceThreeFour = ResonantCollinearity.calculateDistance(pointThree, pointFour)
    
    assertEquals(distanceTwoThree, 1)
    assertEquals(distanceThreeFour, 8)
  }

  test("Verify that I can add two Points") {
    val pointOne = ResonantPoint(7, 7)

    val result = pointOne.add(1, 1)
    assertEquals(result, ResonantPoint(8, 8))
  }

  test("Test height and width of parsedInput") {
    assertEquals(PARSED_INPUT.height, 12)
    assertEquals(PARSED_INPUT.width, 12)
  }

  test("calculate rise over run for two points") {
    val leftPoint = ResonantPoint(5, 7)
    val rightPoint = ResonantPoint(8, 9)

    val (rise, run) = calculateRiseRun(leftPoint, rightPoint)

    assertEquals(3, Math.abs(run))
    assertEquals(2, Math.abs(rise))
  } 

  test("Calculate the number of antinodes from the test") {
    val numAntiNodes = PARSED_INPUT.calculateAllAntiNodes

    assertEquals(numAntiNodes, 14)
  }

  test("Calculate all resounding antinodes from test input") {
    val resoundingSum = PARSED_INPUT.calculateAllResoundingAntiNodes

    assertEquals(resoundingSum, 34)
  }

  val TEST_INPUT =
    """............
    ........0...
    .....0......
    .......0....
    ....0.......
    ......A.....
    ............
    ............
    ........A...
    .........A..
    ............
    ............""".stripMargin
      .split("\n")
      .toList
      .map(_.trim)

  val PARSED_INPUT = ResonantCollinearity.parseInput(TEST_INPUT)
}
