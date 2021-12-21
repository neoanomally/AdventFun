package com.sandersme.advent.model

import com.sandersme.advent.graph.Point

import scala.collection.mutable

class ChitonPathTest extends munit.FunSuite {

  val TEST_INPUT = List(
    "1163751742",
    "1381373672",
    "2136511328",
    "3694931569",
    "7463417111",
    "1319128137",
    "1359912421",
    "3125421639",
    "1293138521",
    "2311944581"
  )

  val TEST_CHITON = ChitonPath.parseInput(TEST_INPUT)

  test("Validate that we can parse the input into a MxM grid") {
    val chitonPath: ChitonPath = ChitonPath.parseInput(TEST_INPUT)
    assertEquals(chitonPath.matrix(2)(3).value, 6)
  }

  test("Validate that we can parse input with into an MxM * extendedSize grid") {
    val chitonPath: ChitonPath = ChitonPath.parseInput(TEST_INPUT, 2)

    val height = chitonPath.matrix.size
    val width = chitonPath.matrix.head.size

    assertEquals(height, width)
    assertEquals(height, 20)
  }

  test("Test the shortest Path between start and end") {
    val fastestRoute = ChitonPath.findShortestRoute(TEST_CHITON)

    assertEquals(fastestRoute.distanceToEnd, Option(40))
  }

  test("Test the shortest path when we've extended the grid by 5") {
    val largeChitonPath = ChitonPath.parseInput(TEST_INPUT, 5)
    val fastestPath = ChitonPath.findShortestRoute(largeChitonPath)

    assertEquals(fastestPath.distanceToEnd, Option(315))
  }

  test("extending the original matrix rows by 5, should have a widith of 50 and height of 10") {
    val valueGrid = TEST_CHITON.matrix.map(_.map(_.value))

    val extendedRows = ChitonPath.extendRows(valueGrid, 5)

    assertEquals(extendedRows.head.size, 50)
    assertEquals(extendedRows.size, 10)
    // Value x=11 should be 2 AND value x=48 should be 8
    assertEquals(extendedRows(0)(11), 2)
    assertEquals(extendedRows(0)(47), 2)
  }

  test("Extending the original valuegrid columns by 5 should give us height of 50 and width of 10") {
    val valueGrid = TEST_CHITON.matrix.map(_.map(_.value))

    val extendedColumns = ChitonPath.extendColumns(valueGrid, 5)

    assertEquals(extendedColumns.size, 50)
    assertEquals(extendedColumns.head.size, 10)

    assertEquals(extendedColumns(47)(0), 7)
    assertEquals(extendedColumns(47)(9), 4)
  }

  test("Extending Columns by 1 and rows by 1 are associative") {
    val valueGrid = TEST_CHITON.matrix.map(_.map(_.value))

    val extendedRowsThenColumsn = ChitonPath.extendColumns(ChitonPath.extendRows(valueGrid, 1), 1)
    val extendedColumnsThenRows = ChitonPath.extendRows(ChitonPath.extendColumns(valueGrid, 1), 1)

    assertEquals(extendedColumnsThenRows, extendedRowsThenColumsn)
  }

}
