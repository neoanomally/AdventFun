package com.sandersme.advent.twentyone.model

import scala.annotation.tailrec

/**
 * What we need to do is have a representation that allows us to describe each of the
 * numbers. Then what we can do is go through the ten inputs and try to map each of the
 * letter's to the original state. We can do this with the letters we know and then dynamically flip which letter's
 * can be mapped and how to map them. We are going to have to implement backgracking as we flip these
 *
 *  The goal is to find the mapping to the original characters until each mapped character
 *  can represent all 10 inputs
 * letters
 */
case class SevenSegment(digitValues: List[Segment], outputValues: List[Segment])

object SevenSegment {

  // TODO: Figure out a method that takes in the entire SevenSegments and uses the digitValues to find the
  // TODO: one solution. Then transform all the output values using the one true mapping
  // TODO: Then map those values to numbers

  case class PossibleMapping(root: Char, possibilities: List[Char])
  case class Mapping(map: Map[Char, Char], mappedResults: List[Segment])
  case class MappingResult(mapping: Mapping, outputNumbers: List[Int])

  def createDynamicMappings(uniqueValues: List[Segment]):  List[PossibleMapping] = {
    // Find the digit that maps to 7
    // find the digit that maps to 1

    val one = uniqueValues.find(_.values.size == 2).get
    val seven = uniqueValues.find(_.values.size == 3).get
    val four = uniqueValues.find(_.values.size == 4).get
    val eight = uniqueValues.find(_.values.size == 7).get

    // So WE can start at A because it's know... Then we can
    // Assign B to a value and C to a value,
    // Then we iterate through the full knownMappings
    // We just need to figure out how to backtrack
    /**
     * Dynamically create every mapping
     */
    val mapsToA: List[Char] = List(seven.values.diff(one.values).head)
    val mapsToBD: List[Char] = four.values.diff(one.values).toList
    val mapstoCF: List[Char] = one.values.toList

    val unknownMappings = eight.values.toList
      .diff(mapsToA ++ mapstoCF ++ mapsToBD)

    val myDynamicMapping: List[PossibleMapping] = List(
      PossibleMapping('a', mapsToA),
      PossibleMapping('b', mapsToBD),
      PossibleMapping('c', mapstoCF),
      PossibleMapping('d', mapsToBD),
      PossibleMapping('e', unknownMappings),
      PossibleMapping('f', mapstoCF),
      PossibleMapping('g', unknownMappings)
    )

    myDynamicMapping
  }


  /**
   * We are going to use this as a base for calling the recursive function.
   * One of the reasons we need to do this is because we want to filter out only good
   * Possibilities. (e.g. where the size of the map contains all root values OR the size == 7)
   *
   * @param possibilities
   * @return
   */
  def generatePossibleMappings(segments: List[Segment]): List[Map[Char, Char]] = {
    val possibleMappings = createDynamicMappings(segments)

    generateAllMappings(possibleMappings, Map.empty)
      .filter(_.size == possibleMappings.size) // We do two checkings making sure that keys and values are unique
      .map(_.map(_.swap)) // We generate the original direction but need to flip them back
      .filter(_.size == possibleMappings.size)
  }

  private final def generateAllMappings(possibleMappings: List[PossibleMapping],
                                  mapBuilt: Map[Char, Char]): List[Map[Char, Char]] = {
    if (possibleMappings.isEmpty) {
      List(mapBuilt)
    } else {
      val headPossibility = possibleMappings.head
      val tailPossibilities = possibleMappings.tail

      headPossibility.possibilities.flatMap{ possibility =>
        val updatedMap = mapBuilt + (headPossibility.root -> possibility)
        generateAllMappings(tailPossibilities, updatedMap)
      }
    }
  }

  def parseInput(input: List[String]): List[SevenSegment] = {
    input.map{ line =>
      val split = line.split("\\|")

      val digits: List[Segment] = split(0)
        .split(' ')
        .toList
        .map(_.trim)
        .filter(_.nonEmpty)
        .map(_.toCharArray.toSet)
        .map(v => Segment.apply(v))

      val outputValues = split(1)
        .split(' ')
        .toList
        .map(_.trim)
        .filter(_.nonEmpty)
        .map(_.toCharArray.toSet)
        .map(v => Segment.apply(v))

      SevenSegment(digits, outputValues)
    }


  }

  // WE need a way to transform value on type of value to another based on some
  // mapping unsure on types
  def transform(mappings: Map[Char, Char], digitValues: List[Segment]): Option[Mapping] = {
    val mappedValues = digitValues.map{ digit =>
      val updatedValues = digit.values.map(v => mappings(v))
      Segment(updatedValues)
    }

    val transformedValues = mappedValues
      .filter(segment => CORE_SEGMENTS.contains(segment))

    if (transformedValues.size == digitValues.size)
      Some(Mapping(mappings, transformedValues))
    else
      None
  }

  def transformAll(allMappings: List[Map[Char, Char]],
                   digitValues: List[Segment]): List[Mapping] = {
    allMappings
      .flatMap(mapping => transform(mapping, digitValues))
  }

  def calculateMappingResult(sevenSegment: SevenSegment): Option[MappingResult] = {
    val generatedMapping: List[Map[Char, Char]] = SevenSegment
      .generatePossibleMappings(sevenSegment.digitValues)
    val transformedResults = SevenSegment.transformAll(generatedMapping, sevenSegment.digitValues)

    // TODO This is kind of ugly, but it chains everything together neatly.
    transformedResults
      .headOption
      .flatMap { mapping =>
        transform(mapping.map, sevenSegment.outputValues)
          .map{ transformed =>
            val numbers = convertToNumber(transformed.mappedResults)
            MappingResult(transformed, numbers)
          }
      }
  }

  def convertToNumber(segment: List[Segment]): List[Int] = {
    segment.map(v => CORE_SEGMENTS(v))
  }
  // Special Segments to validate that a map happens correct


  def fromFrequency(segment: List[Segment]): Map[Char, Char] = {
    val freqMap: List[(Char, Int)] = segment
      .flatMap(_.values)
      .groupBy(identity)
      .map(v => (v._1, v._2.size))
      .toList

    val b = freqMap.find(_._2 == 6).map(_._1).get
    val e = freqMap.find(_._2 == 4).map(_._1).get
    val f = freqMap.find(_._2 == 9).map(_._1).get

    Map('b' -> b, 'e' -> e, 'f' -> f)
  }

  def mappingFromSegmentSlicing(segment: List[Segment]): Map[Char, Char] = {
    // b, e, f
    val frequencyMap = fromFrequency(segment)

    // Find the one, and remove f
    val c: Char = segment.find(_.values.size == 2).get
      .values.diff(Set(frequencyMap('f'))).head
    val a: Char = segment.find(_.values.size == 3)
      .map(_.values).get.diff(Set(c, frequencyMap('f'))).head
    val d: Char = segment.find(_.values.size == 4)
      .map(_.values).get.diff(
      Set(frequencyMap('b'), c, frequencyMap('f'))
    ).head

    val updatedMap = frequencyMap + ('c' -> c) + ('d' -> d) + ('a' -> a)
    val g: Char = segment.find(_.values.size == 7).get
      .values.diff(updatedMap.values.toSet).head

    updatedMap + ('g' -> g)
  }

}