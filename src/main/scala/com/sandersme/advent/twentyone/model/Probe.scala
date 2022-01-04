package com.sandersme.advent.twentyone.model

import scala.annotation.tailrec

import com.sandersme.advent.twentyone.graph._
import com.sandersme.advent.twentyone.model.ProbeLauncher
import com.sandersme.advent.twentyone.model.ProbeLauncher._

case class Probe(xVelocity: Int, yVelocity: Int, hitsTarget: Boolean)

/**
 * This class is used to
 * find the Optimal (X, Y) values.  ProbeMoveOneStep logic has moved to the Probe
 * Launcher logic in separate packets. It's essentially looking at the optimal
 * x and Y distinctively. The Optimal X is one that finds the Boudns starting from
 * bottom to top.
 */
object Probe {

  def apply(x: Int, y: Int, targetBounds: TargetBounds): Probe = {
    val hitsTarget = isHitTarget(x, y, targetBounds)

    Probe(x, y, hitsTarget)
  }


  /**
   * This mehtod will take initial velocities for x & y as well as the target bounds.
   * The starting x, y and step are always 0
   * We check the current x and y and see if they are inbounds or beyond bounds and exit if so.
   * If we haven't made it yet, we increment everything forward. Each X & Y should make it
   * to the bounds at some point. Y will no matter what fail at some point so no issues of infinite
   * recursion there. This is tailrec so it will be rewritten as iterations.
   *
   * @param velocityX This variable doesn't change and is the starting X velocity
   * @param velocityY This variable doesn't change and is the starting Y Velocity
   * @param targetBounds The target area we are using to check if we make it within those bounds
   * @param x Cumulative X Value
   * @param y Cumulative Y value
   * @param step Step number we are on
   * @return Boolean If the probe's trajectory hits the target
   */
  @tailrec
  private[model] def isHitTarget(velocityX: Int, velocityY: Int,  targetBounds: TargetBounds,
                                 x: Int = 0, y: Int = 0,
                                 step: Int = 0): Boolean = {
    val isInBounds = targetBounds.isInBounds(x, y)
    val isBeyondBounds = targetBounds.isBeyondBounds(x, y)

    if(isBeyondBounds || isInBounds) {
      isInBounds
    } else {
      val xDelta = Math.max(velocityX - step, 0)
      val yDelta = velocityY - step
      val stepIncr = step + 1

      isHitTarget(velocityX, velocityY, targetBounds, x + xDelta, y + yDelta, stepIncr)
    }
  }


  // TODO This can be abstracted away for the function isInBounds
  // TODO: Might need to know how many steps we are at this bounds
  // BUT Maybe not since all the Y targets are at the -Y
  @tailrec
  private[model] def findFirstXInBoundsFromPrior(bounds: Bounds, xVelocity: Int): Int = {
    val makesIt = stepXForward(bounds, xVelocity)

    if(makesIt) {
      xVelocity
    } else {
      findFirstXInBoundsFromPrior(bounds, xVelocity + 1)
    }
  }

  // TODO This can probably be abstracted for y as well. For now let's be dumb and simple.
  // To abstract maybe we split bounds into two things or pass in two functions.
  // The should stop for X and Y are two different methods.
  @tailrec
  private[model] def stepXForward(bounds: Bounds, velocity: Int, x: Int = 0, step: Int = 0): Boolean = {
    val currentVelocity = velocity - step
    if (bounds.isXInBounds(x) || currentVelocity <= 0) { // TODO: move this to bounds
      bounds.isXInBounds(x)
    } else {
      val incrX = x + currentVelocity
      val stepIncr = step + 1

      stepXForward(bounds, velocity, incrX, stepIncr)
    }
  }


  // TODO: Right now we are just returning the maxY moving downward
  // One Step. We don't actually
  private[model] def findMaxYInBoundsFromPrior(bounds: Bounds, yVelocity: Int,
                                               maxY: Int = Integer.MIN_VALUE): Int = {
    val (makesIt, upperY) = stepYForward(bounds, yVelocity)

    if(upperY < maxY && makesIt) {
      maxY
    } else {
      val updatedMaxY = if (makesIt)  Math.max(upperY, maxY) else maxY

      findMaxYInBoundsFromPrior(bounds, yVelocity - 1, updatedMaxY)
    }
  }

  @tailrec
  private[model] def stepYForward(bounds: Bounds, velocity: Int,
                                  y: Int = 0, step: Int = 0,
                                  maxY: Int = Integer.MIN_VALUE): (Boolean, Int)= {
    val currentVelocity = velocity - step
    if (bounds.isYInBounds(y) || bounds.isYBeyondBoundry(y)) { // TODO: MOve this to bounds
      (bounds.isYInBounds(y), maxY)
    } else {
      val incrY = y + currentVelocity
      val stepIncr = step + 1
      val updatedMaxY = Math.max(maxY, incrY)

      stepYForward(bounds, velocity, incrY, stepIncr, updatedMaxY)
    }
  }

  /**
   * This only searches when it starts froma  bounds not found
   * TODO: I actually think the starting bounds is always Math.abs(MinY)
   * I'd have to prove it though.  I mean I could just generate a bunch of bounds
   * where Y is some number. I think that might only hold true on negative numbers,
   * ON positive values it might be twice the value.
   */
  @tailrec
  private[model] def findUpperBoundsY(bounds: Bounds, yVelocity: Int,
                                      foundBounds: Boolean = false): Option[Int] = {
    if(foundBounds) {
      Some(yVelocity + 1)
    } else if (bounds.isYBeyondBoundry(yVelocity)) {
      None
    } else {
      val (makesIt, _) = stepYForward(bounds, yVelocity)

      findUpperBoundsY(bounds, yVelocity - 1, makesIt)
    }
  }

  /**
   * This method uses a target bounds and figures out the minimum and maximum X & Y values
   * that we will find all possible starting velocities to try whether or not that the probes
   * will hit there targets. This is an optimization to limit our search space instead of trying
   * to do this while we are creating probes. We know ahead of time what the potential max Y and min x
   * should be and this limits our search space.
   * @param targetBounds The bounds that we know the target lays in ahead of time.
   * @return
   */
  def createProbeBounds(targetBounds: TargetBounds): ProbeBounds = {
    val maxX = targetBounds.maxX
    val minY = targetBounds.minY

    val priorX = (targetBounds.maxX * 0.05).toInt
    val priorY = Math.abs(targetBounds.minY) + 1

    val minX = findFirstXInBoundsFromPrior(targetBounds, priorX)
    val maxY = findUpperBoundsY(targetBounds, priorY)

    if (maxY.isEmpty) throw new Exception("ERROR Couldn't find upperBounds for Y")

    Bounds(minX, maxX, minY, maxY.get)
  }
}
