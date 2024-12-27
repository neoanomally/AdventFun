package com.sandersme.advent.twentyfour

class CrossedWiresTest extends munit.FunSuite {
  test("Test that we can connect all wires") {
    val decimalValue = CROSSED_WIRES.getZBinaryToLong
    val expectedDecimal = 2024L

    assertEquals(decimalValue, expectedDecimal)
  }

  test("Test X and Y Binaries and how they match to Z") {
    val wires = CROSSED_WIRES.connectAllWires

    val xLong = java.lang.Long.parseLong(wires.xBinary, 2)
    val yLong = java.lang.Long.parseLong(wires.yBinary, 2)
    val zLong = java.lang.Long.parseLong(wires.zBinary, 2)

    val xySumBinary = java.lang.Long.toBinaryString(xLong + yLong)


    wires.findCandidateGates
      .toList.sortBy(-_._2)
      .foreach(println)
    // println(wires.xBinary)
    // println(wires.yBinary)
    // println(xLong)
    // println(yLong)
    // println(zLong)
    // println(xySumBinary)
    // Looking at this 
    // z02 needs to be 1 <- gnj AND wpb 
    // What I can do is find the mismatching expected output. 
    // Then find all the instructiosn that lead to the output. 
    // Identify potential gates to swap from those ones. Ideally we find gates to swap
    // that give us the values we need. 
    // println(wires.zBinary.takeRight(xySumBinary.length))
    // println(wires.zBinary.takeRight(xySumBinary.length + 1))
  }

  val TEST_INPUT = """x00: 1
                      x01: 0
                      x02: 1
                      x03: 1
                      x04: 0
                      y00: 1
                      y01: 1
                      y02: 1
                      y03: 1
                      y04: 1

                      ntg XOR fgs -> mjb
                      y02 OR x01 -> tnw
                      kwq OR kpj -> z05
                      x00 OR x03 -> fst
                      tgd XOR rvg -> z01
                      vdt OR tnw -> bfw
                      bfw AND frj -> z10
                      ffh OR nrd -> bqk
                      y00 AND y03 -> djm
                      y03 OR y00 -> psh
                      bqk OR frj -> z08
                      tnw OR fst -> frj
                      gnj AND tgd -> z11
                      bfw XOR mjb -> z00
                      x03 OR x00 -> vdt
                      gnj AND wpb -> z02
                      x04 AND y00 -> kjc
                      djm OR pbm -> qhw
                      nrd AND vdt -> hwm
                      kjc AND fst -> rvg
                      y04 OR y02 -> fgs
                      y01 AND x02 -> pbm
                      ntg OR kjc -> kwq
                      psh XOR fgs -> tgd
                      qhw XOR tgd -> z09
                      pbm OR djm -> kpj
                      x03 XOR y03 -> ffh
                      x00 XOR y04 -> ntg
                      bfw OR bqk -> z06
                      nrd XOR fgs -> wpb
                      frj XOR qhw -> z04
                      bqk OR frj -> z07
                      y03 OR x01 -> nrd
                      hwm AND bqk -> z03
                      tgd XOR rvg -> z12
                      tnw OR pbm -> gnj""".stripMargin.split("\n").toList.map(_.trim)

  
  val CROSSED_WIRES = CrossedWires.parseInput(TEST_INPUT)
}
