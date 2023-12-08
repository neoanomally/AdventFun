package com.sandersme.advent.twentythree.model

class AlmanacTest extends munit.FunSuite {
  test("This is a temporary Test to print out the traversal") {
    val almanac = Almanac.parseInput(TEST_INPUT)
    //    almanac.printMapTraversal
  }

  test("Find that the lowest seed location from TEST input is 35") {
    val almanac = Almanac.parseInput(TEST_INPUT)
    val lowestLocation = almanac.findLowestLocation
    val expectedLocation = 35L

    assertEquals(lowestLocation, expectedLocation)
  }

  test("validate from AlmanacMap that we can find the correct destinations") {
    val almanacRangeOne = AlmanacRange(50, 98, 2)
    val almanacRangeTwo = AlmanacRange(52, 50, 48) 
    val almanacMap = AlmanacMap("", "", List(almanacRangeOne, almanacRangeTwo))

    val seeds: List[Long] = List(79, 14, 55, 13)
    val expectedDestinations: List[Long] = List(81, 14, 57, 13)

    val destinations = seeds.map(seed => almanacMap.translateToDestinationValue(seed))
    assertEquals(destinations, expectedDestinations) 
  }

  test("validate sourceMax for AlmanacRange(50, 98, 2) is 99") {
    val almanacRange = AlmanacRange(50L, 98L, 2L)
    val expectedMax = 99: Long 

    val sourceMax = almanacRange.sourceMax
    assertEquals(sourceMax, expectedMax)
  }

  test("Validate that the difference for AlmanacRange(50, 98, 2) is 48") {
    val almanacRangeOne = AlmanacRange(50L, 98L, 2L)
    val expectedDifferenceOne = 48L

    val almanacRangeTwo = AlmanacRange(52L, 50L, 48L) 
    val expectedDifferenceTwo = -2L

    assertEquals(almanacRangeOne.difference, expectedDifferenceOne)
    assertEquals(almanacRangeTwo.difference, expectedDifferenceTwo)
  }

  test("Known overlapping seed to almanac map with (52 50 48)") {
    val testAlmanacRange = AlmanacRange(52L, 50L, 48L)
    val seed = 55L

    val expectedTranslation = Some(57L)
    val translation = testAlmanacRange.destinationFromSource(seed)
    assertEquals(translation, expectedTranslation)
  }

  test("Known overlapping seed to almanac map, maps to the correct destination node") {
    val almanacRange = AlmanacRange(50L, 98L, 2L)
    val seed = 99L
    val seedTwo = 100L

    val expectedTranslation = Some(51L)
    val expectedTranslationTwo = None // We should actual Option out here
    val translation = almanacRange.destinationFromSource(seed)
    val translationTwo = almanacRange.destinationFromSource(seedTwo)

    assertEquals(translation, expectedTranslation)
    assertEquals(translationTwo, expectedTranslationTwo)
  }

  test("Validate that we can find temperature AlmanacMap") {
    val almanac: Almanac = Almanac.parseInput(TEST_INPUT)

    val temperature = almanac.getAlmanacBySource("temperature")
    val expectedDesintation = "humidity"

    assertEquals(temperature.destination, expectedDesintation)
  }

  test("Validate that we can find the seed locations") {
    val almanac: Almanac = Almanac.parseInput(TEST_INPUT)

    val expectedNumMaps = 7
    val expectedNumSeeds = 4 
    val expectedFirstMapSource = "seed"

    val numMaps = almanac.almanacMaps.size
    val numSeeds = almanac.seeds.values.size
    val firstSource = almanac.almanacMaps.head.source

    assertEquals(numSeeds, expectedNumSeeds)
    assertEquals(numMaps, expectedNumMaps)
    assertEquals(expectedFirstMapSource, firstSource)
  }


  test("Test that we can parse out the source and destination") {
    val testInput = "fertilizer-to-water map:"
    val (source, destination) =  Almanac.parseSourceDestination(testInput)

    val expected = ("fertilizer", "water")

    assertEquals(source, expected._1)  
    assertEquals(destination, expected._2)
  }

  test("Test that we can parse out the almanac maps") {
    val testInput = TEST_INPUT.drop(2) // we only want the maps onwards.

    val almanacMaps = Almanac.parseAlmanacMaps(testInput)
    val expectedRanges = List(AlmanacRange(50, 98, 2), AlmanacRange(52, 50, 48))
    val expectedHead = AlmanacMap("seed", "soil", expectedRanges)

    assertEquals(almanacMaps.head, expectedHead)
    
    val expectedTailRanges = List(AlmanacRange(60, 56, 37), AlmanacRange(56, 93, 4))
    val expectedTail = AlmanacMap("humidity", "location", expectedTailRanges)

    assertEquals(almanacMaps.last, expectedTail)
  }


  val TEST_INPUT: List[String] = """seeds: 79 14 55 13

    seed-to-soil map:
    50 98 2
    52 50 48

    soil-to-fertilizer map:
    0 15 37
    37 52 2
    39 0 15

    fertilizer-to-water map:
    49 53 8
    0 11 42
    42 0 7
    57 7 4

    water-to-light map:
    88 18 7
    18 25 70

    light-to-temperature map:
    45 77 23
    81 45 19
    68 64 13

    temperature-to-humidity map:
    0 69 1
    1 0 69

    humidity-to-location map:
    60 56 37
    56 93 4"""
      .stripMargin
      .split("\n")
      .toList
      .map(_.trim)
    }
