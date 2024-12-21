package com.sandersme.advent.twentyfour

import scala.collection.mutable.PriorityQueue
import com.sandersme.advent.Input.readFromDataResource
import scala.annotation.tailrec

class ReindeerOlympicsTest extends munit.FunSuite {
  test("Validate that I parse the grid into correct places ") {

    val expectedReindeerPoint = TEST_REINDEER.objectAt(1, 15)
    val expectedEndPoint = TEST_REINDEER.objectAt(15, 1)
    val expectedWall = TEST_REINDEER.objectAt(0, 0)

    assertEquals(expectedWall, RObj.RWall)
    assertEquals(expectedReindeerPoint, RObj.Start)
    assertEquals(expectedEndPoint, RObj.End)
  }

  test("Scoring Mechanism countOne Step") {
    val oneStepAFalse = ReindeerOlympics.isOneStep(Direction.Left, Direction.Right)
    val oneStepATrue = ReindeerOlympics.isOneStep(Direction.Left, Direction.Down) 
    val oneStepBTrue = ReindeerOlympics.isOneStep(Direction.Left, Direction.Up) 
    val oneStepCTrue = ReindeerOlympics.isOneStep(Direction.Right, Direction.Down) 
    val oneStepDTrue = ReindeerOlympics.isOneStep(Direction.Right, Direction.Up) 

    assertEquals(oneStepAFalse, false)
    assertEquals(oneStepATrue, true)
    assertEquals(oneStepBTrue, true)
    assertEquals(oneStepCTrue, true)
    assertEquals(oneStepDTrue, true)
  }

  test("Test that we can traverse the grid for minimum score of 11048") {
    val results = TEST_REINDEER 

    assertEquals(results.calculateScore, 11048L)
  }

  test("Print off the path with which we traversed".ignore) {
    val results = TEST_REINDEER.startTraverse 


    // val pointDir = results.map(v => v.p -> v.facing).toMap

    // TEST_REINDEER.grid.zipWithIndex.foreach { case (line, y) => 
    //   line.zipWithIndex.foreach { case (obj, x) => obj match 
    //     case RObj.RWall => print('#')
    //     case _ if pointDir.contains(Point(x, y)) => pointDir(Point(x, y)) match {
    //       case Direction.Up => print('^')
    //       case Direction.Right => print('>')
    //       case Direction.Down => print('v')
    //       case Direction.Left => print('<')
    //     }
    //     case _ => print('.')
    //   }
    //   println("")
    // }
  }


  test("Start grid 3") {
    val res =  TEST_REINDEER.startGrid3
    val numTiles = TEST_REINDEER.countGrid3Tiles
    assertEquals(numTiles, 64)
  }

  test("Start grid 6") {

    val sixResults =TEST_REINDEER.startTraverseSixDFSThree
    


    val a = Point(7, 10)
    val b = Point(13, 10)
    val c = Point(15, 7)
    val oneFour = Point(1, 4)
    val threeFour = Point(3, 4)
    val oneFive = Point(1, 5)
    val threeFive = Point(3, 5)
    val end = Point(15, 1)
    val other = Point(10, 11)
    val otherother = Point(11, 11)
    val shouldHaveTwo = Point(15, 7)
    // println("Final 15, 7 entry: " + sixResults._1(shouldHaveTwo))

    // println(sixResults._1(oneFour))
    // println(sixResults._1(oneFive))
    // println(sixResults._1(threeFour))

    // println("A " + sixResults._1(a))
    // println("END: " + sixResults._1(end))
    // println("A " + sixResults._1(a))
    // println("B " + sixResults._1(b))
    // println("Contains A: " + sixResults._2(a) + "\tContains B: " + sixResults._2.contains(b))
    // println("C: " + sixResults._1(c))
    // println("GRID SIX: " +  sixResults._2.size)

    // sixResults._1.foreach(println)
    // sixResults._2.toList.sortWith((l, r) => if (l.x == r.x) l.y < r.y else l.x < r.x).foreach(println)
    // assertEquals(sixResults._2.size, 64)
  } 

  test("do a depth first search to find the min path from every node should have 64 tiles") {
    val numTiles = TEST_REINDEER.countNumTiles

    val gridRes = TEST_REINDEER.startGrid2
    
    assertEquals(numTiles, 64)

  }

  test("Assert that I can calculate part two: should equal 45" ) { 
    val results = TEST_REINDEER.calculateScoreTwo

  }

  val TEST_INPUT = """#################
        #...#...#...#..E#
        #.#.#.#.#.#.#.#.#
        #.#.#.#...#...#.#
        #.#.#.#.###.#.#.#
        #...#.#.#.....#.#
        #.#.#.#.#.#####.#
        #.#...#.#.#.....#
        #.#.#####.#.###.#
        #.#.#.......#...#
        #.#.###.#####.###
        #.#.#...#.....#.#
        #.#.#.#####.###.#
        #.#.#.........#.#
        #.#.#.#########.#
        #S#.............#
        #################"""
    .stripMargin
    .split("\n")
    .toList
    .map(_.trim)

//   val TEST_INPUT = """###############
// #.......#....E#
// #.#.###.#.###.#
// #.....#.#...#.#
// #.###.#####.#.#
// #.#.#.......#.#
// #.#.#####.###.#
// #...........#.#
// ###.#.#####.#.#
// #...#.....#.#.#
// #.#.#.###.#.#.#
// #.....#...#.#.#
// #.###.#.#.#.#.#
// #S..#.....#...#
// ###############""".stripMargin.split("\n").toList.map(_.trim)
    val TEST_REINDEER = ReindeerOlympics.parseInput(TEST_INPUT)
}
