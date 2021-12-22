package com.sandersme.advent.algorithm

import com.sandersme.advent.algorithm.RateLimiter
import com.sandersme.advent.model.Response
import com.sandersme.advent.model.Response

import scala.collection.mutable
import java.util.Collections
import java.util.concurrent.{ConcurrentLinkedQueue, LinkedBlockingDeque}

/**
 * TODO Look at actual algorithms.
 * @param limitSize
 * @param durationInMs
 */
class RateLimiter(private val limitSize: Int, private val durationInMs: Long) {
  private[algorithm] val leakyBucket: ConcurrentLinkedQueue[Long] =
    new ConcurrentLinkedQueue[Long]()


  // We store monotonically increasing values:
  // 1, 2, 3, 3, 3, 4, 5
  // timeDiff will be newValue - Difference so if the diff is 5 and the new value is 7
  // Then 7 - 5 = 2, remove all values less than 2
  // We want to remove all values less than that difference.
  private def updateLeakyBucket(timeDiff: Long): Int = {

    if (!leakyBucket.isEmpty && leakyBucket.peek() < timeDiff) {
      leakyBucket.removeIf(_ < timeDiff)
    }

    leakyBucket.size
  }

  /**
   * TODO: Figure out how I would do this for real.
   * @param time
   * @return
   */
  def shouldAllow(time: Long): Response = this.synchronized {
    val limitDiff = time - durationInMs
    val size = updateLeakyBucket(limitDiff)

    if (size < limitSize) {
      leakyBucket.add(time)
      Response.PASS
    } else {
      Response.FAIL
    }
  }
}

/**
 * TODO: I would actually like the ability
 * to define a given to summon a rate Limiter. Or some default one.
 */
object RateLimiter {
  def apply(limitSize: Int, durationInMs: Long): RateLimiter = {
    new RateLimiter(limitSize, durationInMs)
  }

  // TODO: Instead of returning a ratelimiter itself. It might be useful
  // To amake a function that returns a function that returns either the
  // value or an error based on ratelimiter.
}
