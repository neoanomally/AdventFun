package com.sandersme.advent.twentyfour

class MullItOverTest extends munit.FunSuite {
  test("TEST that we can parse and then sum all the matmuls. Should equal 161") {
    val processor = MulProcessor.create(TEST_INPUT)
    
    val sum = processor.sum

    val expectedSum = 161

    assertEquals(sum, expectedSum)
  }

  /// I check each letter one at a time until I come across a mul(, then I check if is Int
  /// up to three characters and then a comma, then I check again for up to three
  //  characters and then a closing parenth
  //  States: finding mul enters in mul state
  //          in mul state finding integer up til comma 
  //          comma state finding integer up until parenthesis
  //
  //          once fully found add the two integers to an object

  val TEST_INPUT = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
}
