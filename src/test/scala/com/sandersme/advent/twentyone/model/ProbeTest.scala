package com.sandersme.advent.twentyone.model

import com.sandersme.advent.twentyone.graph.Bounds

import com.sandersme.advent.twentyone.graph.TargetBounds
import com.sandersme.advent.twentyone.graph.ProbeBounds


class ProbeTest extends munit.FunSuite {

  test("Step forward with X until it can't step or reached bounds.") {
    val bounds = Bounds(20, 30, 40, 100)
    val madeItIn = Probe.stepXForward(bounds, 6)
    val didntMakeItIn = Probe.stepXForward(bounds, 3)

    assert(madeItIn)
    assert(!didntMakeItIn)
  }

  test("X Value Step forward to find out if x is in bounds staring from velocity 1 until 10") {
    val bounds = Bounds(20, 30, 40, 100)
    val minX = Probe.findFirstXInBoundsFromPrior(bounds, 3)

    assertEquals(minX, 6)
  }

  test("Y Value step forward until it's in or out of bounds") {
    val bounds = Bounds(20, 30, -10, -5)
    val outOfBoundsResults = Probe.stepYForward(bounds, 10)
    val expectedOutOfBoundsResults = (false, 55)

    val inBoundsResults = Probe.stepYForward(bounds, 9)
    val expectedInBoundsResults = (true, 45)

    assertEquals(outOfBoundsResults, expectedOutOfBoundsResults)
    assertEquals(inBoundsResults, inBoundsResults)
  }

  test("Y Value try multiple values starting from 15, decrement till 9 to find the max Y") {
    val bounds = Bounds(20, 30, -10, -5)
    val startingY = 15
    val results = Probe.findMaxYInBoundsFromPrior(bounds, 15)

    assertEquals(results, 45)
  }

  test("Finds upper bound from bounds. Should equal to 9") {
    val bounds = Bounds(20, 30, -10, -5)
    val startingY = 15

    val results = Probe.findUpperBoundsY(bounds, startingY)

    assertEquals(results, Some(9))
  }

  test("Find upper Bound from LOW starting position") {
    val bounds = Bounds(20, 30, -10, -5)
    val startingY = -55

    val results = Probe.findUpperBoundsY(bounds, startingY)

    assertEquals(results, None)
  }

  test("Create Probe Bounds from target bounds") {
    val targetBounds: TargetBounds = Bounds(20, 30, -10, -5)
    val expectedBounds: ProbeBounds = Bounds(6, 30, -10, 9)

    val results = Probe.createProbeBounds(targetBounds)
    assertEquals(results, expectedBounds)
  }

  // The following two tests are really just syntactic sugar for testing the isHitTargetTest
  test("Check if probe is in bounds based on test data. We know 6,3 should be in bounds") {
    val targetBounds: TargetBounds = Bounds(20, 30, -10, -5)
    val probe: Probe = Probe(6, 3, targetBounds)
    val expectedProbe: Probe = Probe(6, 3, true)

    assertEquals(probe, expectedProbe)
  }

  test("Check if probe is in bounds based on test data. We know that 6, -3 will quickly go out of bounds") {
    val targetBounds: TargetBounds = Bounds(20, 30, -10, -5)
    val probe: Probe = Probe(6, -3, targetBounds)
    val expectedProbe: Probe = Probe(6, -3, false)

    assertEquals(probe, expectedProbe)

  }
}
