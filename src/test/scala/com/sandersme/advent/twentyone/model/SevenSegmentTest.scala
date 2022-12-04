package com.sandersme.advent.twentyone.model

import com.sandersme.advent.Input
import com.sandersme.advent.twentyone.model.SevenSegment.Mapping

class SevenSegmentTest extends munit.FunSuite {
  val TEST_INPUT: List[String] =
                   """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
                     |edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
                     |fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
                     |fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
                     |aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
                     |fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
                     |dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
                     |bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
                     |egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
                     |gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagcee
                     |""".stripMargin.split("\n").toList

  val DEFAULT_SEVEN_SEGMENTS: List[SevenSegment] = SevenSegment.parseInput(TEST_INPUT)

  test("Validate that we can parse the data into our data model") {
    val parsedInput: List[SevenSegment] = SevenSegment.parseInput(TEST_INPUT)

    val averageSizeOfLeftSegments: Double = parsedInput
      .map(_.digitValues.size).sum.toDouble / parsedInput.size

    assertEquals(averageSizeOfLeftSegments, 10.0)
  }

  test("Create Dynamic Mappings to make sure they get created well") {
    val dynamicMappings = SevenSegment.createDynamicMappings(CORE_SEGMENTS.keys.toList)
    val possibleMapping = SevenSegment.generatePossibleMappings(CORE_SEGMENTS.keys.toList)

    val transform = possibleMapping.map(mapping =>
      SevenSegment.transform(mapping, CORE_SEGMENTS.keys.toList))
  }

  test("Validate we have created various mapping potentials") {
    val inputSegments: List[Segment] = DEFAULT_SEVEN_SEGMENTS.head.digitValues
    val possibleMappings = SevenSegment.generatePossibleMappings(inputSegments)

    val onlyOneTypeOfMapping = possibleMappings
      .map(_.size)

    assertEquals(onlyOneTypeOfMapping.groupBy(identity).size, 1)
    assertEquals(onlyOneTypeOfMapping.size, 8)
  }

  test("Transform From mappings that are generated from finding all possible mappings") {
    val head = DEFAULT_SEVEN_SEGMENTS.head

    val inputSegments: List[Segment] = head.digitValues
    val possibleMappings = SevenSegment.generatePossibleMappings(inputSegments)


    val test: Seq[Mapping] = SevenSegment.transformAll(possibleMappings, inputSegments)
    val numbers: Seq[Int] = SevenSegment.convertToNumber(test.head.mappedResults).sorted
    val expectedValues = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

    assertEquals(numbers, expectedValues)
  }

  test("Transform  CORE_DATA create mappings and then transform should convert back.") {
    val generatedPossibilites = SevenSegment.generatePossibleMappings(CORE_SEGMENTS.keys.toList)

    val v = SevenSegment.transformAll(generatedPossibilites, CORE_SEGMENTS.keys.toList)
    val transform = generatedPossibilites.flatMap(possibility =>
      SevenSegment.transform(possibility, CORE_SEGMENTS.keys.toList))

    assertEquals(transform.size, 1)
    assertEquals(v.size, 1)
  }

  test("Validate Core Data contains") {
    val testValidations = List(Segment(Set('c', 'd', 'f', 'b' )), Segment(Set('a', 'c', 'd', 'f', 'g')))

    val shouldBeTrue = testValidations.map(CORE_SEGMENTS.contains).forall(identity)
    assertEquals(shouldBeTrue, true)
  }

  test("TEST_INPUT transformation... Let's go through each one and find the possible mappings") {
    DEFAULT_SEVEN_SEGMENTS
      .map { segments =>
        val digits = segments.digitValues

        val mappings = SevenSegment.generatePossibleMappings(digits)
        val transformations = SevenSegment.transformAll(mappings, digits)
        transformations
      }
  }

  // THIS PASSED PART 1 WTF
  test("Test something that I hate about myself because I don't read well.") {

    val input = Input.readTwentyOneFromResource("day8_input")
    val inputSevenSegment = SevenSegment.parseInput(input)

    val outputValues = inputSevenSegment
      .flatMap(_.outputValues)
      .flatMap(_.values.size match {
        case 2 => Some(1)
        case 3 => Some(7)
        case 4 => Some(4)
        case 7 => Some(8)
        case _ => None
      }).size
  }

  test("I suck WOW") {
    //               8                       7                  4         1
    val t = List("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")
    val parsed = SevenSegment.parseInput(t)

    val mappings = SevenSegment.generatePossibleMappings(parsed.head.digitValues)
    val transformAll = SevenSegment.transformAll(mappings, parsed.head.digitValues)

    val transformToDigits: List[Int] = SevenSegment
      .convertToNumber(transformAll.head.mappedResults)

    assertEquals(transformToDigits.size, 10)
  }

  test("Created Mapped Results from the original containers") {
    val mappingResult = SevenSegment.calculateMappingResult(DEFAULT_SEVEN_SEGMENTS.head)

    val results: String = mappingResult.head
      .outputNumbers
      .foldLeft("")((res, v) => res + v)

    assertEquals(results, "8394")
  }

  test("Let's count things for mappings") {
    val coreSegments: List[Segment] = CORE_SEGMENTS.keys.toList

    val charCounts: Map[Char, Int] = coreSegments
      .flatMap(_.values.toList)
      .groupBy(k => k)
      .map(k => k._1 -> k._2.size)

    val sumMapping: List[(Int, Int)] = CORE_SEGMENTS.toList
      .map{ case(segment, mapping) =>
        val sum: Int = segment.values.map(charCounts).sum
        sum -> mapping
      }.sortBy(_._2)
  }


}
