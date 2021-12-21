package com.sandersme.advent.algorithm

import com.sandersme.advent.algorithm.RateLimiter.Response._

class RateLimiterTest extends munit.FunSuite {

  test("Testing out a Rate Limiter") {
    val rateLimiter: RateLimiter = RateLimiter(10, 20000)
    def startTime  = 1000000L

    // the first 10 values should pass, then the next 10 fail, then next 10 pass, then rest fail
    val results = (0 to 33).foldLeft(List.empty[RateLimiter.Response]) { (accum, i) =>
      val time: Long = if (i < 20) {
        startTime
      } else if( i <= 31) {
        startTime + 100000L
      } else {
        startTime + 400000L
      }
      val limiter = RateLimiter.shouldLimit(rateLimiter, time)

      accum :+ limiter._2
    }

    val expected = List(PASS, PASS, PASS, PASS, PASS, PASS, PASS, PASS, PASS, PASS, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL,
      FAIL, FAIL, FAIL, FAIL, PASS, PASS, PASS, PASS, PASS, PASS, PASS, PASS, PASS, FAIL, FAIL, FAIL, PASS, PASS)
    assertEquals(results, expected)
  }

}
