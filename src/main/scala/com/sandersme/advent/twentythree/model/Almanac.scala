package com.sandersme.advent.twentythree.model

import scala.annotation.tailrec
import com.sandersme.advent.twentytwo.model.PacketType.startParsing

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
}

case class Seeds(values: List[Long])


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
    * @return
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
