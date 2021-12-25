package com.sandersme.advent.model

import com.sandersme.advent.graph.Bounds

class ProbeLauncherTest extends munit.FunSuite {
  test("Parse target area: x=20..30, y=-10..-5 into a bounds") {
    val probeLauncher: ProbeLauncher = ProbeLauncher.parseInput("target area: x=20..30, y=-10..-5")

    assertEquals(probeLauncher.targetBounds.maxX, 30)
    assertEquals(probeLauncher.targetBounds.minY, -10)
    assertEquals(probeLauncher.targetBounds.maxY, -5)
  }

  test("Find all possible targets from input target bounds") {
    val probeLauncher: ProbeLauncher = ProbeLauncher.parseInput("target area: x=20..30, y=-10..-5")
    val allPossibleValidProbes = probeLauncher.findAllPossibleProbes
    val expectedNumberOfPossibleProbes = 112

    assertEquals(allPossibleValidProbes.size, expectedNumberOfPossibleProbes)
  }
}
