package com.sandersme.advent.twentyfour

class DiskFragmenterTest extends  munit.FunSuite {
  test("Validate that I can parse the input into a DiskFragmenter object") {
    val expected = Vector(File(Vector(Segment(0, 2)), 0), File(Vector.empty, 3), File(Vector(Segment(1, 3)), 0),
      File(Vector.empty, 3), File(Vector(Segment(2, 1)), 0), File(Vector.empty, 3), File(Vector(Segment(3, 3)), 0))

    assertEquals(PARSED_INPUT.files.take(expected.length), expected)
  }

  test("validate that I can delete the space from a file and turn it into free sapce") {
    val testFile = File(Vector(Segment(9, 2), Segment(13, 4)), 1)

    val deleteSpace = testFile.deleteSpace
    val expected = File(Vector.empty, 7)

    assertEquals(deleteSpace, expected)
  }

  test("validate that I can add a segment to file") {
    val testFile = File(Vector(Segment(9, 2), Segment(13, 4)), 1)
    val segmentToAdd = Segment(14, 1)
    val (addedSegment, remainingSegment) = testFile.addSegment(segmentToAdd)

    val expected = File(Vector(Segment(9, 2), Segment(13, 4), Segment(14, 1)), 0)
    assertEquals(addedSegment, expected) 
    assertEquals(remainingSegment, None)

    val (_, remainingSegmentIdentity) = addedSegment.addSegment(segmentToAdd)
    assertEquals(remainingSegmentIdentity, Some(segmentToAdd))
  }

  test("Validate that defrag one step works") {

    val afterOneStep = DiskFragmenter.defragOneStep(PARSED_INPUT)
    val afterTwoStep = DiskFragmenter.defragOneStep(afterOneStep)

    val expectedFileIndexOneAfterFirstStep = File(Vector(Segment(9, 2)), 1)
    val expectedFileIndexOneAfterTwoSteps = File(Vector(Segment(9, 2), Segment(8, 1)), 0)
    assertNotEquals(PARSED_INPUT.files(1), expectedFileIndexOneAfterFirstStep)
    assertEquals(afterOneStep.files(1), expectedFileIndexOneAfterFirstStep)
    assertEquals(afterTwoStep.files(1), expectedFileIndexOneAfterTwoSteps)
  }

  test("Defrag full disk") {
    val finishedDisk = DiskFragmenter.defragFullDisk(PARSED_INPUT, 1)
    
    assertEquals(finishedDisk.filesIdx, finishedDisk.freeSpaceIdx)
  
  }

  test("The test Checksum should equal 1928".ignore) {
    val finishedDisk = DiskFragmenter.defragFullDisk(PARSED_INPUT, 1)

    val checkSum = finishedDisk.calculateCheckSum

    // val checkSum = finishedDisk.calculateResultFromFinalString
    assertEquals(checkSum, BigInt(1928))
  }

  test("verify that we can defrag part 2") {
    val finishedDisk = DiskFragmenter.defragPartTwo(PARSED_INPUT, PARSED_INPUT.files.length - 1)
    val expected = "00992111777.44.333....5555.6666.....8888.."

    assertEquals(finishedDisk.createFinalString, expected)
  }

  test("print the final string such that it's equal to the expected output") {
    val expected = "0099811188827773336446555566.............."
    val finishedDisk = DiskFragmenter.defragFullDisk(PARSED_INPUT, 1)

    val finishedString = finishedDisk.createFinalString
    assertEquals(finishedString, expected)
  }

  test("Assert Checksum for defrag 2 is 2858") {
    val finishedDisk = DiskFragmenter.defragPartTwo(PARSED_INPUT, PARSED_INPUT.files.length - 1)
    val checkSum = finishedDisk.calculateCheckSum
    println(finishedDisk.createFinalString)
    val expectedCheckSum = BigInt(2858)
    assertEquals(checkSum, expectedCheckSum)

  }

  val TEST_INPUT = """2333133121414131402""".stripMargin.split("\n").toList


  val PARSED_INPUT = DiskFragmenter.parseInput(TEST_INPUT)
}
