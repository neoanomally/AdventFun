package com.sandersme.advent.twentyone.model

import com.sandersme.advent.Input
import com.sandersme.advent.twentyone.binary.{One, Zero}

class ImageEnhancementTest extends munit.FunSuite {
  val data: List[String] = Input.readTwentyOneFromResource("day20_testinput")
  val TEST_IMAGE_ENHANCEMENT: ImageEnhancement = ImageEnhancement.parseInput(data)

  test("Validate that parse input works as expected.") {
    assertEquals(TEST_IMAGE_ENHANCEMENT.algorithm.size, 512)
    assertEquals(TEST_IMAGE_ENHANCEMENT.algorithm(2), true)
    assertEquals(TEST_IMAGE_ENHANCEMENT.algorithm.last, true)
    assertEquals(TEST_IMAGE_ENHANCEMENT.algorithm.dropRight(1).last, false)
    assertEquals(TEST_IMAGE_ENHANCEMENT.activePixelCount, 10)
    assertEquals(TEST_IMAGE_ENHANCEMENT.activePixels.contains((0, 0)), false)
    assertEquals(TEST_IMAGE_ENHANCEMENT.activePixels.contains((6, 7)), true)
    assertEquals(TEST_IMAGE_ENHANCEMENT.activePixels.contains((7, 9)), true)
  }

  test("values to check witha  (0, 0) should equal 18)") {
    val resultsA = ImageEnhancement.getAlgorithmIndex((0,0), TEST_IMAGE_ENHANCEMENT)
    val resultsB = ImageEnhancement.getAlgorithmIndex((9, 5), TEST_IMAGE_ENHANCEMENT)
    assertEquals(resultsA, 0)
    assertEquals(resultsB, 32)
  }

  test("Update the pixel value for (0, 0), which should look up index 18 which should return None") {
    val updatedPixel = ImageEnhancement.updateImageLocation((0, 0), TEST_IMAGE_ENHANCEMENT)

    assertEquals(updatedPixel, None)
  }

  test("Validate get potential centers is size 13 and includes (0, 0) itself") {
    val origin = (0, 0)

    val allPotentialNeighbors = ImageEnhancement.getLocationPotentialNeighbors(origin)

    assertEquals(allPotentialNeighbors.size, 25)
    assertEquals(allPotentialNeighbors.contains(origin), true)
  }

  /**
   * TODO: For the main code base ALTERNATIVELY we can do the pixel math on each 13 ahead of time
   * so that we limit HOW Much goes into memory; but this is a good starting step for testing.
   */
  test("""Get ALL potential activiations for the next iteration. This should be a Set.""") {
    // TEST Where the neighbors dont' overlap
    val testImageEnhancementA = ImageEnhancement.apply(Set((0, 0), (10, 10)), Vector.empty)
    val allActiveNeighborsA = ImageEnhancement.getAllActiveNeighbors(testImageEnhancementA)
    // TEST where nneighbors are going to overlap by three so we should have
    val testImageEnhancementB = ImageEnhancement.apply(Set((0, 0), (2, 2)), Vector.empty)
    val allActiveNeighborsB = ImageEnhancement.getAllActiveNeighbors(testImageEnhancementB)

    assertEquals(allActiveNeighborsA.size, 50)
    assertEquals(allActiveNeighborsB.size, 41)
  }

  test("Apply image enhancement twice should equal 35") {
    val updatedTwice: ImageEnhancement = TEST_IMAGE_ENHANCEMENT
      .applyEnhancementAlgorithm(2)

    assertEquals(updatedTwice.activePixelCount, 35)
  }

  test("Min and max dimensions for the test boarder should return {X, Y} {X, Y}") {
    val dimensions = TEST_IMAGE_ENHANCEMENT.borderDimensions
    val expectedDimensions = MinMaxDimensions(5, 5, 9, 9)
    assertEquals(dimensions, expectedDimensions)
  }

  test("Check if the border / infinity is active") {
    // Set the first value of the algorithm to true. This is to have the test case when infinity
    // Nodes are blinking e.g. 000000000 => True
    val updatedAlgorithm =  true +: TEST_IMAGE_ENHANCEMENT.algorithm.tail
    val testImageEnhanmentUpdated = TEST_IMAGE_ENHANCEMENT.copy(algorithm = updatedAlgorithm)

    val firstIteration =  testImageEnhanmentUpdated.applyEnhancementAlgorithm(1)
    val secondIteration = firstIteration.applyEnhancementAlgorithm(1)
    val isBorderActiveZeroIteration = testImageEnhanmentUpdated.isBorderActive

    val isBorderActiveFirstIteration = firstIteration.isBorderActive


    assertEquals(isBorderActiveZeroIteration, false)
    assertEquals(firstIteration.isBorderActive, true)
    assertEquals(secondIteration.isBorderActive, false)
  }

  test("Check if if a location is beyond the border dimensions.") {
    val firstIterationOriginal = TEST_IMAGE_ENHANCEMENT.applyEnhancementAlgorithm(1)
    val shouldNotBeActiveByeondBorderOriginal = firstIterationOriginal.isActive(1, 1)
    val shouldNotBeActiveBeyondBorderOriginalB = firstIterationOriginal.isActive(12, 13)

    val firstIterationWithPixelatedAlgorithm = firstIterationOriginal
      .copy(algorithm = true +: firstIterationOriginal.algorithm.tail)
    val shouldBeActiveBeyondBorderPixelated = firstIterationWithPixelatedAlgorithm.isActive(1, 1)
    val shouldBeActiveBeyondBorderPixelatedB = firstIterationWithPixelatedAlgorithm.isActive(12, 13)

    val border = firstIterationWithPixelatedAlgorithm.borderDimensions

    assertEquals(shouldNotBeActiveBeyondBorderOriginalB, Zero)
    assertEquals(shouldNotBeActiveByeondBorderOriginal, Zero)
    assertEquals(shouldBeActiveBeyondBorderPixelatedB, One)
    assertEquals(shouldBeActiveBeyondBorderPixelated, One)
  }

  test("IsBeyondBorder check different bordder conditions") {
    val border = TEST_IMAGE_ENHANCEMENT.borderDimensions

    assertEquals(TEST_IMAGE_ENHANCEMENT.isBeyondBorder(5, 5), false)
    assertEquals(TEST_IMAGE_ENHANCEMENT.isBeyondBorder(9, 9), false)
    assertEquals(TEST_IMAGE_ENHANCEMENT.isBeyondBorder(12, 8), true)
    assertEquals(TEST_IMAGE_ENHANCEMENT.isBeyondBorder(5, 5), false)
  }

  test("Full day20_input results are 5291") {
    val fullInput = Input.readTwentyOneFromResource("day20_input")
    val imageEnhancement = ImageEnhancement.parseInput(fullInput)
    val twiceEnhanced = imageEnhancement.applyEnhancementAlgorithm(2)

    assertEquals(twiceEnhanced.activePixelCount, 5291)
  }
}
