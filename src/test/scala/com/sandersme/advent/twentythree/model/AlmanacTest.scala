package com.sandersme.advent.twentythree.model

import com.sandersme.advent.twentythree.model.Almanac.parseAlmanacMaps

class AlmanacTest extends munit.FunSuite {
  test("This is a temporary Test to print out the traversal") {
    val almanac = Almanac.parseInput(TEST_INPUT)
    // println(almanac.countTotalNumberSeeds)
    //    almanac.printMapTraversal
  }

  test("Test the seed ranges map to the correct values") {
    val seedRangeLeft = SeedRange(79, 92)
    val seedRangeRight = SeedRange(55, 67)
    val bothSeedRanges = List(seedRangeLeft, seedRangeRight)

    val maps = Almanac.parseInput(TEST_INPUT)
    val firstMap = maps.almanacMaps.head

    val leftTranslated = firstMap.ranges.head.splitSeedRangesTranslated(seedRangeLeft)
    val rightTranslated = firstMap.ranges.head.splitSeedRangesTranslated(seedRangeRight)
    val translated = firstMap.translateSeedRangeToDestination(bothSeedRanges)
   
    val expectedLeftTranslated = (List(SeedRange(79, 92)), None)
    val expectedRightTranslated = (List(SeedRange(55, 67)), None)
    
    assertEquals(leftTranslated, expectedLeftTranslated)
    assertEquals(rightTranslated, expectedRightTranslated)
  }

  test("Find the final seed range locations using test data") {
    val almanac = Almanac.parseInput(TEST_INPUT)

    val seedRangeLocations = almanac.findSeedRangeLocations 
    val expectedMin = 46L
    val min = almanac.findSeedRangeMinLocation
    assertEquals(min, expectedMin)
  }


  // TODO if we split up too many ranges we may actually need to merge them at the end. 
  test("translateSeedRangeToDestination goes over the set of seed translation for a single Map") {
    val almanac = Almanac.parseInput(TEST_INPUT)
    val translatedSeeds = almanac.almanacMaps
      .head.translateSeedRangeToDestination(almanac.seeds.createSeedRanges)
    // TODO Write a real test
  }

  test("Take an almanacRange and map the destination") {
    val almanacRange = AlmanacRange(0, 10, 10)
    val seedRange = SeedRange(13, 20)

    val expectedSeedRange = SeedRange(3, 10)
    val translatedSeedRange = almanacRange.destinationFromRange(seedRange)
    assertEquals(translatedSeedRange, expectedSeedRange)
  }

  test("Take an almanacRange and split seedrange into remaining Ranges and Processed Ranges") {
    val almanacRange = AlmanacRange(0, 10, 10)
    val seedRange = SeedRange(0, 30) // should have 0 - 9, 21 - 30: 10-20

    val (remaining, processed) = almanacRange.splitSeedRange(seedRange)

    val expectedRemaining = List(SeedRange(0, 9), SeedRange(20, 30))
    val expectedProcessed = Option(SeedRange(10, 19))

    assertEquals(remaining, expectedRemaining)
    assertEquals(processed, expectedProcessed)
  }

  test("Validate that seed ranges are valid ranges e.g. start is < stop and vicee versa") {
    val seedRangeValid  = SeedRange.validateSeedRange(SeedRange(5, 10))
    val seedRangeValidAtPoint = SeedRange.validateSeedRange(SeedRange(5,5))
    val seedRangeInvalidA =  SeedRange.validateSeedRange(SeedRange(21, 5))
    val seedRangeInvalidB =  SeedRange.validateSeedRange(SeedRange(5, -1))

    assertEquals(seedRangeValid, Some(SeedRange(5, 10)))
    assertEquals(seedRangeInvalidB, None)
    assertEquals(seedRangeValidAtPoint, Some(SeedRange(5, 5)))
    assertEquals(seedRangeInvalidB, None)
  }

  // TODO FInd a funciton that turns SeedRange into three chunks:
  // 1. What's overlapping (Math.min(range.start, seed.stop)) (Math.max(range.stop,
  //    seed.start))
  // 2. What's left below (e.g. seed.stop < range.start)
  // 3. What's left above (e.g. seed.start > range.stop
  // Combine steps 2 & 3 into remainingRanges. and then put 1 into the new range.

  test("Check if seed range is in AlchemyRange returns the correct values.") {
    val almanacRange = AlmanacRange(0, 10, 11) // this means the values are 10-20 
    val seedRangeAIsInRange = SeedRange(5, 15) // this means values are 5 - 15
    val seedRangeBIsInRange = SeedRange(15, 25) // this means the values are 15 - 25
    val seedRangeCIsNotInRange = SeedRange(0, 5) // below values
    val seedRangeDIsNotInRange = SeedRange(21, 26) // above values


    assertEquals(almanacRange.isSeedRangeInRange(seedRangeAIsInRange), true)
    assertEquals(almanacRange.isSeedRangeInRange(seedRangeBIsInRange), true)
    assertEquals(almanacRange.isSeedRangeInRange(seedRangeCIsNotInRange), false)
    assertEquals(almanacRange.isSeedRangeInRange(seedRangeDIsNotInRange), false)
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


  test("Create various Seed range pairs from the input") {
    val seeds = Seeds(List(79L, 14L, 55L, 13L))

    val expectedSeedRanges = List(SeedRange(79L, 92L), SeedRange(55L, 67L))
    val seedRanges = seeds.createSeedRanges

    assertEquals(seedRanges, expectedSeedRanges)
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
