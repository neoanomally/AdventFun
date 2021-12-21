package com.sandersme.advent.algorithm

import com.sandersme.advent.algorithm.RateLimiter.Response
import org.scalacheck.Test.Failed

import scala.annotation.tailrec
import scala.collection.mutable
import java.util.Collections
import java.util.concurrent.LinkedBlockingDeque
import scala.concurrent.duration.Duration

/**
 * TODO update this with an AtomicReference
 * @param limitSize
 * @param durationInMs
 */
class RateLimiter(private val limitSize: Long, private val durationInMs: Long) {
  private[algorithm] val deque: LinkedBlockingDeque[Long] = new LinkedBlockingDeque[Long]()
  private[algorithm] val leakyBucket: mutable.ListBuffer[Long] = {
    new mutable.ListBuffer[Long]()
  }


  // We store monitonically increasing values:
  // 1, 2, 3, 3, 3, 4, 5
  // timeDiff will be newValue - Difference so if the diff is 5 and the new value is 7
  // Then 7 - 5 = 2, remove all values less than 2
  // We want to remove all values less than that difference.
  private def updateLeakyBucket(timeDiff: Long): Int = {
    if (leakyBucket.nonEmpty && leakyBucket.head < timeDiff) {
      val removeSize = leakyBucket.takeWhile(_ < timeDiff).size
      leakyBucket.remove(0, removeSize - 1)
    }

    leakyBucket.size
  }

  /**
   * TODO: Figure out how we would model returnign both RateLimiter and Response
   * @param time
   * @return
   */
  def limit(time: Long): RateLimiter = synchronized {
    val limitDiff = time - durationInMs
    val size = updateLeakyBucket(limitDiff)

    if (size <= limitSize)
      leakyBucket.append(time)
      this
    else {
      this
    }
  }

  private[algorithm] def shouldAllow: Response = {
    if (leakyBucket.size.toLong <= limitSize) {
      Response.PASS
    } else {
      Response.FAIL
    }
  }
}

object RateLimiter {
  def apply(limitSize: Long, durationInMs: Long): RateLimiter = {
    new RateLimiter(limitSize, durationInMs)
  }

  enum Response {
    case PASS, FAIL, EMPTY
  }

  def shouldLimit(rateLimiter: RateLimiter, time: Long): (RateLimiter, Response) = {
    (rateLimiter.limit(time), rateLimiter.shouldAllow)
  }
}
