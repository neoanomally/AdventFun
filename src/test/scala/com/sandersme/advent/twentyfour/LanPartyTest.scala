package com.sandersme.advent.twentyfour

class LanPartyTest extends munit.FunSuite {
  test("Test to make sure that the network graph parses correctly") {
    val res = LAN_PARTY.findThreesStartT

    val expected = Set(
     Set("co","de","ta"),
     Set( "co","ka","ta"),
     Set( "de","ka","ta"),
     Set( "qp","td","wh"),
     Set( "tb","vc","wq"),
     Set( "tc","td","wh"),
     Set( "td","wh","yn"))

    assertEquals(expected, res)
  }

  test("Find all interconnectednetworks") {
    val interconnected = LAN_PARTY.findLargestInterConnectedNetwork

    assertEquals(interconnected, List("co", "de", "ka", "ta"))
  }


  test("Test the set intersection of two graphs filter neighborhood candidates") {
    val graph = Map("a" -> Set("b", "c", "d"),
      "b" -> Set("d", "c", "e"),
      "c" -> Set("d", "b", "e"),
      "d" -> Set("b", "c"),
      "e" -> Set("f")
    )

    val fir = LanParty.filterNeighborCandidates("a", Set("b", "c"), Set("b", "c", "d"))
    val flr = LanParty.filterNeighborCandidates("a", Set("b", "c", "d"), Set("b", "c", "e"))
    
    assertEquals(fir, Set("b", "c"))
    assertEquals(flr, Set("b", "c"))
  }

  val TEST_INPUT = """kh-tc
        qp-kh
        de-cg
        ka-co
        yn-aq
        qp-ub
        cg-tb
        vc-aq
        tb-ka
        wh-tc
        yn-cg
        kh-ub
        ta-co
        de-co
        tc-td
        tb-wq
        wh-td
        ta-ka
        td-qp
        aq-cg
        wq-ub
        ub-vc
        de-ta
        wq-aq
        wq-vc
        wh-yn
        ka-de
        kh-ta
        co-tc
        wh-qp
        tb-vc
        td-yn""".stripMargin.split("\n").toList.map(_.trim)

  val LAN_PARTY = LanParty.parseInput(TEST_INPUT)
}
