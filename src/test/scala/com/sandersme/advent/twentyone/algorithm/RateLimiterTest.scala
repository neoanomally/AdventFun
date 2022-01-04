package com.sandersme.advent.twentyone.algorithm

import com.sandersme.advent.twentyone.model.Response
import com.sandersme.advent.twentyone.model.Response._

class RateLimiterTest extends munit.FunSuite {

  test("Testing out a Rate Limiter") {
    val rateLimiter: RateLimiter = RateLimiter(10, 20000)
    def startTime  = 1000000L

    // the first 10 values should pass, then the next 10 fail, then next 10 pass, then rest fail
    val results = (0 to 33).foldLeft(List.empty[Response]) { (accum, i) =>
      val time: Long = if (i < 20) {
        startTime
      } else if( i <= 31) {
        startTime + 100000L
      } else {
        startTime + 400000L
      }
      val shouldLimit = rateLimiter.shouldAllow(time)

      accum :+ shouldLimit
    }

    val expected = List(PASS, PASS, PASS, PASS, PASS, PASS, PASS, PASS, PASS, PASS, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL,
      FAIL, FAIL, FAIL, FAIL, PASS, PASS, PASS, PASS, PASS, PASS, PASS, PASS, PASS, PASS, FAIL, FAIL, PASS, PASS)
    assertEquals(results, expected)
  }

  test("Let me manually Test some stuff".ignore) {
    val limiter = RateLimiter(5, 1000)

    val accumulator: List[Response] = (1 to 50).foldLeft(List.empty[Response]){(accum, _) =>
      val updatedAccum = accum :+ limiter.shouldAllow(System.currentTimeMillis())
      Thread.sleep(50)
      updatedAccum
    }
  }

}
