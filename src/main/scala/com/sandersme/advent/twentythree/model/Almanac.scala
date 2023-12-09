package com.sandersme.advent.twentythree.model

import scala.annotation.tailrec

/**
  * This is a container for the input seeds and a collection of all the maps @see AlmondMap.
  *
  * @param seeds
  * @param almanacMaps
  */
case class Almanac(seeds: Seeds, almanacMaps: List[AlmanacMap]) {
  // The following two variables are the starting and entry points of the map
  val ENTRY = "seed"
  val TERMINATION = "location"

  // The following will error out if there is no field in the map.
  // This behavior is fine if this is just for advent of code.
  def getAlmanacBySource(source: String): AlmanacMap = {
    almanacMaps.find(_.source == source).get
  }

  def getAlmanacByDestination(destination: String): AlmanacMap = {
    almanacMaps.find(_.destination == destination).get
  }

  /**
    * This is a helper method used to print off the map traversal this is used for
    * debugging purposes. 
    */
  def printMapTraversal: Unit = {
    val startingPoint: AlmanacMap = getAlmanacBySource(ENTRY)

    def loop(map: AlmanacMap): Unit = {
      println(s"Source: ${map.source}\tDestination: ${map.destination}")

      if (map.destination == TERMINATION)
        println("END")
      else {
        val nextMap = getAlmanacBySource(map.destination)
        loop(nextMap)
      }
    }

    loop(startingPoint)
  }

/**
  * This is method will go through the list of seed ranges and map them to the locations
  * should return a list of translated seed ranges that are mapped to the final location
  *
  * @return
  */  
  def findSeedRangeLocations: List[SeedRange] = {
    val startingMap: AlmanacMap = getAlmanacBySource(ENTRY)

    @tailrec
    def loop(almanacMap: AlmanacMap, seedRanges: List[SeedRange]): List[SeedRange] = {
      val translatedValues = almanacMap.translateSeedRangeToDestination(seedRanges)
      if (almanacMap.destination == TERMINATION) 
        translatedValues
      else
        val nextMap = getAlmanacBySource(almanacMap.destination)
        loop(nextMap, translatedValues)
    }

    // println(s"Created Seed Ranges: ${seeds.createSeedRanges}")
    loop(startingMap, seeds.createSeedRanges)
  }

  def findSeedRangeMinLocation: Long = {
    findSeedRangeLocations
      .map(_.start)
      .min
  }

  /**
    *  This method hops from source to destination until it finds 'location' as it's
    *  source. Each step along the way it translates it's starting value using the
    *  AlmanacMap Ranges else identity function. 
    */
   def findSeedLocations: List[Long] = {
     def startingMap: AlmanacMap = getAlmanacBySource(ENTRY)

     @tailrec
     def loop(almanacMap: AlmanacMap, currentValues: List[Long]): List [Long] = {
       val translatedValues = currentValues.map(almanacMap.translateToDestinationValue)
       
       if(almanacMap.destination == TERMINATION)
         translatedValues
       else
         val nextMap = getAlmanacBySource(almanacMap.destination)
         loop(nextMap, translatedValues)
     }

     loop(startingMap, seeds.values)
   }

   def findLowestLocation: Long = {
    findSeedLocations.min
   }

   def countTotalNumberSeeds: Long = {
     seeds.createSeedRanges
       .map(_.spanLength)
       .sum
   }
}


case class SeedRange(start: Long, stop: Long) {
  def spanLength: Long = stop - start + 1
}

object SeedRange {
  def fromSpanLength(start: Long, spanLength: Long): SeedRange = {
    SeedRange(start, start + spanLength - 1)
  }

  def validateSeedRange(seedRange: SeedRange): Option[SeedRange] = {
    if( seedRange.start > seedRange.stop)
      None 
    else if (seedRange.stop < seedRange.start)
      None
    else 
      Some(seedRange)
  }
}

case class AlmanacRange(destinationStart: Long, sourceStart: Long, rangeLength: Long) {
  /**
    * This method checks to see if the seed input value is within the source range of
    * this given AlmanacRange. This is done by comparing hte source start and range
    * length. If so it the seed can be mapped to the destination. 
    *
    * @param value
    * @return Boolean indicating that the value can be mapped from source to destination
    */
  def inSourceRange(seed: Long): Boolean = {
    sourceStart <= seed && seed <= sourceMax
  }
  
/**
  * The following method translates a seedrange from source to destination. Warning there
  * is no guarantee within this method that the translation will put you in the correct
  * range due to being over the span. this is done in a different method.
  *
  * Ideally in a real world situation I would either error out or send the maximum. 
  *
  * @param seedRange
  * @return
  */
  def destinationFromRange(seedRange: SeedRange): SeedRange = {
    SeedRange(seedRange.start - difference, seedRange.stop - difference)
  }

  def splitSeedRangesTranslated(seedRange: SeedRange): (List[SeedRange], Option[SeedRange]) = {
    val (remaining, seedInRange) = splitSeedRange(seedRange)
    val translated = seedInRange.map(destinationFromRange)

    (remaining, translated)
  }

  /**
    * Get all the values below the mappingFromSegmentSlicing() 
    *
    * @param seedRange
    * @return
    */
  def splitSeedRange(seedRange: SeedRange): (List[SeedRange], Option[SeedRange]) = {
    // 79 97 with a Range => 98, 99
    // get all values below   (seed.min - map.start - 1) 
    // get all values aboe (map.sourceMax + 1 - seed.max) 
    // get the intersection of all values Math.min stuff.
    //
    val allValuesBelow = SeedRange(seedRange.start, Math.min(seedRange.stop, sourceStart - 1))
    val allValuesAbove = SeedRange(Math.max(sourceMax + 1, seedRange.start), seedRange.stop)

    val intersectionStart = Math.max(sourceStart, seedRange.start)
    val intersectionStop = Math.min(sourceMax, seedRange.stop)
    val allValuesBetween = SeedRange(intersectionStart, intersectionStop)

    val validatedBetween = SeedRange.validateSeedRange(allValuesBetween)
    
    val stillNeedProcessing = List(allValuesBelow, allValuesAbove)
      .flatMap(SeedRange.validateSeedRange)

    (stillNeedProcessing, validatedBetween)
  }

  def isSeedRangeInRange(seedRange: SeedRange): Boolean = {
    // 0, 5 || 10, 20
    val isStartBetween = seedRange.start >= sourceStart && seedRange.start <= sourceMax
    val isStopBetween = seedRange.stop >= sourceStart && seedRange.stop <= sourceMax
    isStartBetween || isStopBetween
  }

  /**
    * This is a convenience method that calculates the sourceMax
    * The example is source range starts at 98 with range 2, so should have values 98 & 99
    * @return
    */
  def sourceMax: Long = sourceStart + rangeLength - 1
  
  /**
    * This value is used to map the current value to the start 
    *
    * @return
    */
  def difference: Long = sourceStart - destinationStart

  /**
    *  This takes in a seed value and maps it to the desintation value
    *  For example if we have 98 we should reach desintation 50
    * @param seed
    * @return
    */
  def destinationFromSource(seed: Long): Option[Long] = {
    if (inSourceRange(seed)) {
      Some(seed - difference)
    } else {
      None
    }
  }
}

/**
  * Case class that holds the source and desination identifier. Then it contains the list
  * of AlmanacRanges
  *
  * @param source
  * @param destination
  * @param ranges
  */
case class AlmanacMap(source: String, destination: String, ranges: List[AlmanacRange]) {
  /**
    * Given a seed number map to the correct destination number. This is done by 
    * calling {@link AlmanacRanges.findDestination}
    *
    * @param seed Input seed number to find destination for.
    * @return destination number
    */
  def translateToDestinationValue(seed: Long): Long = {
    ranges
      .find(_.inSourceRange(seed))
      .flatMap(_.destinationFromSource(seed))
      .getOrElse(seed)
  }

  // This method goes over every Range. Then it processes the list of seed ranges not
  // found. At every step we split out any non-overlapping seed ranges that fit within the
  // range. We keep an accumulation of seeds that are remaining to be processed and the
  // successfully translated ranges. Finally at the end we update to the final method. 
  def translateSeedRangeToDestination(seedRanges: List[SeedRange]): List[SeedRange] = {
    val defaultTranslatedRanges = List.empty[SeedRange]
    val accumulator = (seedRanges, defaultTranslatedRanges)
    val (nonTranslatedSeeds, translatedSeeds) = ranges
      .foldLeft(accumulator){ case((seeds, translated), range) => 
        val splitAndTranslated = seeds
          .map(range.splitSeedRangesTranslated)
        
        val seedRangesRemaining = splitAndTranslated.flatMap(_._1)
        val translatedSeedRanges = splitAndTranslated.flatMap(_._2)
        val accumulated = translated ++ translatedSeedRanges

        (seedRangesRemaining, accumulated)
      }

    nonTranslatedSeeds ++ translatedSeeds
  }
}

case class Seeds(values: List[Long]) {
   def createSeedRanges: List[SeedRange] = {
      @tailrec
      def loop(remaining: List[Long], pairs: List[SeedRange]): List[SeedRange] = {
        if (remaining.isEmpty)
          pairs
        else {
          val left = remaining.head
          val right = remaining.tail.head
          val updatedPairs = pairs :+ SeedRange.fromSpanLength(left, right)

          loop(remaining.drop(2), updatedPairs)
        }
      }

      loop(values, List.empty)
   }
}


object Almanac {
  /**
      * The Input is going to consist of a serieis of data with some repeating
      * line. Between each of the steps below .
      * 1. This is going to contain a list of Longs for Seeds. This is always one line:
      *    seeds: List(...)
      * 2. seed-to-soil map: Then it has 1..n lines three space deliminated values
      * 3. soil-to-fertalizer map.  1..n lines of three space deliminated values 
      * 4. fertilizer-to-water map  1..n lines of three space deliminated values
      * 5. water-to-light map       1..n lines of three space deliminated values
      * 6. light-to-temperature     1..n lines of three space deliminated values
      * 7. temperature-to-humidity  1..n lines of three space deliminated values
      * 8. humidity-to-location     1..n lines of thre space deliminated values
      * So we can parse each map by first split on the space; then split on -to-
      * then we can store those as keys and values in a container. po
      * 
      * Column 1: Destination Range
      * Column 2: Source Range
      * Column 3: Range length. 
      *
      * @param input
      * @return
      */
  def parseInput(input: List[String]): Almanac = {
    val seeds = parseSeeds(input.head)

    val tail = input.drop(2)
    val almanacMaps = parseAlmanacMaps(tail)
    Almanac(seeds, almanacMaps)
  }

  /**
      * This method will only parse the list of seeds from the first line of the input.
      *
      * @param input
      * @return
      */
  def parseSeeds(input: String): Seeds = {
    val parsed: List[Long] = input.drop(7)
      .split(" ")
      .map(_.toLong)
      .toList

    Seeds(parsed)
  }

  /**
    * Take in the remaining list that should start with the first line "seed-to-soil" THis
    * is going to fold Left accumulating a SeedMap. 
    *
    * @param input
    * @return The result is a list of the AlmanacMaps containing the lists of ranges as
    * well as the source and destination in a case class. 
    */
  def parseAlmanacMaps(input: List[String]): List[AlmanacMap] = {
    @tailrec
    def loop(rest: List[String], accum: List[AlmanacMap]): List[AlmanacMap] = {
      if (rest.size == 0) {
        accum
      } else {
        val head::tail = rest
        
        val rangeInput = tail.takeWhile(!_.isBlank())
        val remaining = tail.drop(rangeInput.size + 1)
        val (source, destination) = parseSourceDestination(head)

        val ranges = rangeInput.foldLeft(List.empty[AlmanacRange]){ case(ranges, line) =>
          ranges :+ parseRange(line) 
        }

        val updatedAccum = accum :+ AlmanacMap(source, destination, ranges)

        loop(remaining, updatedAccum)
      }
    }

    loop(input, List.empty)
  }

  /**
      * Parse out the source and destination from the string
      *
      * @param input should have format of 'souce'-to-'destination' map
      * @return a string pair of the source and destination
      */
  def parseSourceDestination(input: String): (String, String) = {
    val split = input.split(" ")
      (0).split("-to-")
  
    (split(0), split(1))
  }

  def parseRange(input: String): AlmanacRange = {
    val split = input
      .split(" ")
      .map(_.toLong)

    assert(split.size == 3)

    AlmanacRange(split(0), split(1), split(2))
  }
}
