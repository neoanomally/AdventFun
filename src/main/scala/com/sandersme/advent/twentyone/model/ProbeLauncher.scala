package com.sandersme.advent.twentyone.model

import com.sandersme.advent.twentyone.graph._

import scala.annotation.tailrec

case class ProbeLauncher(targetBounds: TargetBounds, probeBounds: ProbeBounds) {
  def findLegalMaxY: Int = {
    // Trying to create a smart prior for our starting step location.
    // BEcause this is an unbounded problem to figure out the peak Y
    val priorY: Int = Math.abs(targetBounds.minY)
    Probe.findMaxYInBoundsFromPrior(targetBounds, priorY)
  }

  /**
   * Method that takes the TargetBounds and the ProbeBounds. For every possible combination
   * of x & y values create a probe using the target bounds. Filter out any probe that doesn't
   * hit the target and keep the probes that do hit the target.
   *
   * This relies on using all the power from the probe to figure out if it will hit the target or not
   * a nice divide and conquer :)
   * @return The list of possible probes that will hit the taret.  q
   */
  def findAllPossibleProbes: List[Probe] = {
    val validProbes = for {
      x <- (probeBounds.minX to probeBounds.maxX)
      y <- (probeBounds.minY to probeBounds.maxY)

      probe = Probe(x, y, targetBounds)

      if (probe.hitsTarget)
    } yield probe

    validProbes.toList
  }
}

object ProbeLauncher {

  /**
   *
   * @param input String containing Target area: eg. `target area: x=20..30, y=-10..-5`
   * @return Probe Launcher with the targetBounds and the ProbeBounds
   */
  def parseInput(input: String): ProbeLauncher = {
    val targetBounds: TargetBounds  = Bounds.targetBoundsFromInput(input)
    val probeBounds: ProbeBounds = Probe.createProbeBounds(targetBounds)

    ProbeLauncher(targetBounds, probeBounds)
  }
}
