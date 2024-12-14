package com.sandersme.advent.twentyfour

class ClawContraptionTest extends munit.FunSuite {
  test("Validate that parsing the claw machine works") {
    val headMachine = CLAW_CONTRAPTION.machines.head
    val expectedMachine = ClawMachine(CPoint(94, 34), CPoint(22, 67), CPoint(8400, 5400))

    assertEquals(headMachine, expectedMachine)
  }

  test("Validate that the greatest common denominator can or can't be found") {
    val headMachine = CLAW_CONTRAPTION.machines.head

    val gcdX = headMachine.gcdX
    val gcdY = headMachine.gcdY
    assertEquals(gcdX, BigInt(2))
    assertEquals(gcdY, BigInt(1))
  }

  test("Validate that we can find all combinations of multiples for claw machine") {
    val a = ClawMachine.findCombinations(94, 22, 8400)
    val b = ClawMachine.findCombinations(34, 67, 5400)

    val headMachine = CLAW_CONTRAPTION.machines.head
    val aExpected = Set((69, 87), (14, 322), (36, 228), (25, 275), (80, 40), (47, 181), (3, 369), (58, 134))
      .map(v => (BigInt(v._1), BigInt(v._2)))
    val bExpected = Set((13, 74), (80, 40), (147, 6))
      .map(v => (BigInt(v._1), BigInt(v._2)))

    assertEquals(a, aExpected)
    assertEquals(b, bExpected)
  }

  test("Find all factors for 100") {
    val factors = ClawMachine.findFactors(BigInt(100))

    val results = List(2, 2, 5, 5)
  }

  test("Find all factors for 10000000006450") {
    val factors = ClawMachine.findFactors(BigInt(10000000006450L))

    println(factors)
  }

  test("Solve for part 2") {
    val part2Results = CLAW_CONTRAPTION.findPart2

    assertEquals(part2Results, BigInt(875318608908L))
  }

  test("Validate that we can optimal num tokens should equal: 480") {

    val headMachine = CLAW_CONTRAPTION.machines.head
    val f = headMachine.optimizeToPrize

    assertEquals(f, BigInt(280))
  }

  test("Run min tokens for a prize for all machines") {
    val minTokens = CLAW_CONTRAPTION.findAllOptimizations

    assertEquals(minTokens, BigInt(480))
  }

  test("Validate that the optimization equals 80 ") {
    val headMachine = CLAW_CONTRAPTION.machines.head

    val optimization = ClawMachine.findCombinations(headMachine.a.x, headMachine.b.x, headMachine.prize.x)
    assertEquals(optimization.map(_._1).filter(_ == BigInt(80)).head, BigInt(80))
  }


  val TEST_INPUT = """Button A: X+94, Y+34
      Button B: X+22, Y+67
      Prize: X=8400, Y=5400

      Button A: X+26, Y+66
      Button B: X+67, Y+21
      Prize: X=12748, Y=12176

      Button A: X+17, Y+86
      Button B: X+84, Y+37
      Prize: X=7870, Y=6450

      Button A: X+69, Y+23
      Button B: X+27, Y+71
      Prize: X=18641, Y=10279""".stripMargin
    .split("\n")
    .toList
    .map(_.trim)

    val CLAW_CONTRAPTION = ClawContraption.parseInput(TEST_INPUT)
}
