package com.sandersme.advent.twentyfour

class RestroomRedoubtTest extends munit.FunSuite {
  test("some kind of test here") {
    val headRobot = PARSED_INPUT.robots.head

    assertEquals(headRobot, Robot(RPoint(0, 4), RPoint(3, -3)))
  }

  test("Check to make sure that basic move one step works") {

    val headRobot = PARSED_INPUT.robots.head
    val moved = headRobot.move(PARSED_INPUT.width, PARSED_INPUT.height)
    val expectedRobot = Robot(RPoint(3, 1), RPoint(3, -3))

    assertEquals(moved, expectedRobot)
  }

  test("Check to make sure that the move and wrap is working as intended for the test") {
    val robot = Robot(RPoint(2, 4), RPoint(2, -3)) // p=2,4 v=2,-3
    val afterFiveMoves = (0 until 5).foldLeft(robot){ case (updatedRobot, _) => 
      updatedRobot.move(11, 7)
    }

    // val robot2 = PARSED_INPUT.robots(5)
    // val afterFiveMoves2 = (0 until 5).foldLeft(robot2){ case (updatedRobot, _) => 
    //   
    //   val res = updatedRobot.move(11, 7)
    //   println(res)
    //   res
    // }

    
    // (0 until 9).foldLeft(PARSED_INPUT){ case (bathroom, next) => 
    //   println("Iteration: " + next + "\n")
    //   bathroom.printTiles
    //   println(bathroom.robots)
    //   bathroom.moveAllRobots(1)
    // }

    val expectedEndPosition = RPoint(1, 3)
    assertEquals(afterFiveMoves.point ,expectedEndPosition)
  }

  test("Move all robots 5 times and validate their end positions".ignore) {
    val updatedBathroom = PARSED_INPUT.moveAllRobots(4)
    val expected = List(RPoint(6, 0), RPoint(6, 0), RPoint(9, 0), RPoint(0, 2), RPoint(1, 3), RPoint(2, 3), RPoint(5, 4), RPoint(4, 6), RPoint(5, 6), RPoint(5,6), RPoint(1, 7), RPoint(7, 7))
    

    val sortedBathroomRobots = updatedBathroom.robots.map(_.point).sortWith((l, r) => {
      if (l.x == r.x)
        l.y < r.y
      else 
        l.x < r.x
    })

    val sortedExpected = expected.sortWith((l, r) => {
      if (l.x == r.x)
        l.y < r.y 
      else 
        l.x < r.x
    })

    assertEquals(sortedBathroomRobots, sortedExpected) 
  }

  test("Move all robots 100 times and count number in each of the four quadrants") {
    val updatedBathRoom = PARSED_INPUT.moveAllRobots(100)

    val expectedSafteyScore = 12
    assertEquals(updatedBathRoom.calculateSafetyScore, expectedSafteyScore)
  }

  // val TEST_INPUT_2 = """p=2,4 v=2,2""".stripMargin.split("\n").toList

  val TEST_INPUT = """p=0,4 v=3,-3
      p=6,3 v=-1,-3
      p=10,3 v=-1,2
      p=2,0 v=2,-1
      p=0,0 v=1,3
      p=3,0 v=-2,-2
      p=7,6 v=-1,-3
      p=3,0 v=-1,-2
      p=9,3 v=2,3
      p=7,3 v=-1,2
      p=2,4 v=2,-3
      p=9,5 v=-3,-3""".stripMargin
    .split("\n")
    .toList
    .map(_.trim)

  val PARSED_INPUT = RestroomRedoubt.parseInput(TEST_INPUT).copy(width = 11, height = 7)
}
