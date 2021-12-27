package com.sandersme.advent.model

class SnailFishNumberTest extends munit.FunSuite {
  val TEST_LARGE_HOMEWORK_ASSIGNMENT = List("[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]",
    "[[[5,[2,8]],4],[5,[[9,9],0]]]",
    "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]",
    "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]",
    "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]",
    "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]",
    "[[[[5,4],[7,7]],8],[[8,3],8]]",
    "[[9,3],[[9,9],[6,[4,9]]]]",
    "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]",
    "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]")


  val TEST_SNAILFISH_NUMBER: SnailFishNumber =
    SnailFishNumber.parsePairs("[[[3,[2,3]],[[8,0],2]],[0,[[8,1],[7,0]]]]")

  test("Validate we can match on union type for SnailfishNumber") {
    val testNumber = SnailFishNumber(5, SnailFishNumber(3, 5))


    val (rightLeft, rightRight) = testNumber.right match {
      case a: Int => (a, 0)
      case pair: SnailFishNumber => (pair._1, pair._2)
    }

    assertEquals(rightLeft, 3)
    assertEquals(rightRight, 5)
  }

  test("Validate a list of IntOrPair will return the right results as a Pair") {
    val testInput: List[Char | SnailFishNumber] = List('6', ',', SnailFishNumber(5, 3))
    val results = SnailFishNumber.parseInnerBracket(testInput)
    val expectedValues = SnailFishNumber(SnailFishNumber(5, 3), 6)

    val twoSnailFishNumbers: List[Char | SnailFishNumber] = List(SnailFishNumber(5, 2), SnailFishNumber(6, 3))
    val resultsTwoSnailFishNumbers = SnailFishNumber.parseInnerBracket(twoSnailFishNumbers)
    val expectedTwoSnailFishNumber = SnailFishNumber(SnailFishNumber(6, 3), SnailFishNumber(5, 2))

    assertEquals(results, expectedValues)
    assertEquals(resultsTwoSnailFishNumbers, expectedTwoSnailFishNumber)
  }

  test("Test the function untilOpenBracket on a List that's being used as a Stack") {
    val stack: List[Int | Char | SnailFishNumber] = List(6, SnailFishNumber(4, 3), '[', SnailFishNumber(4, 3))
    val subList = stack.takeWhile(SnailFishNumber.untilOpenBracket)

    val expected = stack.take(2)

    assertEquals(subList, expected)
  }


  // [[[3,[2,3]],[[8,0],2]],[0,[[8,1],[7,0]]]]
  // becomes:            ,[[3,2[,3[[[
  test("Let's test the order of operations ") {
    val expectedValue = sfn(sfn(sfn(3, sfn(2, 3)), sfn(sfn(8, 0), 2)), sfn(0, sfn(sfn(8, 1), sfn(7, 0))))

    assertEquals(TEST_SNAILFISH_NUMBER, expectedValue)
  }

  test("Check to see if we can find the max depth of a branch") {
    val depth = SnailFishNumber.findDepth(TEST_SNAILFISH_NUMBER)
    val willExplode = SnailFishNumber.willExplodeFromRoot(TEST_SNAILFISH_NUMBER)
    val maxValue    = SnailFishNumber.maxNumberInTree(TEST_SNAILFISH_NUMBER)

    assertEquals(depth, 4)
    assertEquals(willExplode, false)
    assertEquals(maxValue, 8)
  }

  test("Validate a reduce explode action") {
    val exampleExplosion = SnailFishNumber.parsePairs( "[[[[[9,8],1],2],3],4]")

    val explodeAndReduced = exampleExplosion.reduce
    // EXPECTED VALUE = [[[[0,9],2],3],4]

    val expectedValue = sfn(sfn(sfn(sfn(0, 9), 2), 3), 4)

    assertEquals(SnailFishNumber.willExplodeFromRoot(exampleExplosion), true)
    assertEquals(SnailFishNumber.willExplodeFromRoot(explodeAndReduced), false)

    assertEquals(explodeAndReduced, expectedValue)
  }


  test("Find the depth of each branch for left and right") {
    val inputEvenBothSides = SnailFishNumber.parsePairs("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")

    val results = SnailFishNumber.findRootBranchDepth(inputEvenBothSides)

    assertEquals(results, (5, 5))
  }

  test("Validate a reduce split action") {
    val splitTen: SnailFishNumber | Int = SnailFishNumber.splitNumber(10)
    val splitEleven: SnailFishNumber | Int = SnailFishNumber.splitNumber(11)

    assertEquals(splitTen, SnailFishNumber(5, 5))
    assertEquals(splitEleven, SnailFishNumber(5, 6))
  }

  test("Recursive Reduce on input that should have two phases of reduce") {
    val input = SnailFishNumber.parsePairs("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")
    val expectedResults = SnailFishNumber.parsePairs("[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
    val results = SnailFishNumber.recursiveReduce(input)

    assertEquals(results, expectedResults)
  }

  test("Parse input, which converts each line into a list of pairs, with no reduction") {
    val fullInput = List("[1,1]", "[2,2]", "[3,3]", "[4,4]")
    val finalSnailFishNumber = SnailFishNumber.parseInput(fullInput)
    val expectedValue = SnailFishNumber.parsePairs("[[[[1,1],[2,2]],[3,3]],[4,4]]")

    assertEquals(finalSnailFishNumber, expectedValue)
  }

  //TODO; is there some error here? I don't get how the outcome becomes 3, 0
  test("Parse input, which converts each line into a list of pairs, reducing to final SnailFishNumber") {
    val fullInput = List("[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]")
    val finalSnailFishNumber = SnailFishNumber.parseInput(fullInput)

    val expectedValue = SnailFishNumber.parsePairs("[[[[3,0],[5,3]],[4,4]],[5,5]]")

    assertEquals(finalSnailFishNumber, expectedValue)
  }

  // HOW DOES THIS ONE WORK BUT NOT THE ABOVE ONE :O
  test("Parse input and should have two reductions on the last two items") {
      val fullInput = List("[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]", "[6,6]")
      val finalSnailFishNumber = SnailFishNumber.parseInput(fullInput)

      val expectedValue = SnailFishNumber.parsePairs("[[[[5,0],[7,4]],[5,5]],[6,6]]")

      assertEquals(finalSnailFishNumber, expectedValue)
    }

  test("Test larger input of additions merges etc") {
    val largerInput = List(
      "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
      "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
      "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
      "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]", // Once we get here we are in an infinite loop
      "[7,[5,[[3,8],[1,4]]]]",
      "[[2,[2,2]],[8,[8,1]]]",
      "[2,9]",
      "[1,[[[9,3],9],[[9,0],[0,7]]]]",
      "[[[5,[7,4]],7],1]",
      "[[[[4,2],2],6],[8,7]]"
    )

    val snailFishNumber = SnailFishNumber.parseInput(largerInput)
    val expected = SnailFishNumber.parsePairs("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")

    assertEquals(snailFishNumber, expected)
  }

  test("A simpler exmaple that will have multiple steps, the first is to add them together then explode + Split") {
    val input = List("[[[[4,3],4],4],[7,[[8,4],9]]]",  "[1,1]")
    val parsedInput = SnailFishNumber.parseInput(input)

    val expected = SnailFishNumber.parsePairs("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
    assertEquals(parsedInput, expected)
  }

  test("Snailfish, split with multiple values above 10") {
    val input = sfn(sfn(10, 15), 15)

    val firstSplitSnailFish = SnailFishNumber.split(input)
    val secondSplitSnailFish = SnailFishNumber.split(firstSplitSnailFish)
    val firstSplitExpected =    sfn(sfn(sfn(5, 5), 15), 15)
    val expectedSplitExpected = sfn(sfn(sfn(5, 5), sfn(7, 8)), 15)

    assertEquals(firstSplitSnailFish, firstSplitExpected)
    assertEquals(secondSplitSnailFish, expectedSplitExpected)

  }

  test("Testing will explode from various different children") {
    val input: SnailFishNumber = SnailFishNumber.parsePairs("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")
    val inputB: SnailFishNumber = SnailFishNumber.parsePairs("[7,[6,[5,[4,[3,2]]]]]")
    val willExplodeFromRoot = SnailFishNumber.willExplode(input, 1)

    val willExplodeFromLeftA = SnailFishNumber.willExplode(input.left, 2)
    val willExplodeFromRightA = SnailFishNumber.willExplode(input.right, 2)
    val willExplodeFromLeftB = SnailFishNumber.willExplode(inputB.left, 2)
    val willExplodeFromRightB = SnailFishNumber.willExplode(inputB.right, 2)

    assertEquals(willExplodeFromRoot, true)
    assertEquals(willExplodeFromLeftA, true)
    assertEquals(willExplodeFromRightA, true)
    assertEquals(willExplodeFromRightB, true)
    assertEquals(willExplodeFromLeftB, false)

  }

  test("Snailfish, validate explode . The left most value should explode and update values") {
    import com.sandersme.advent.model.SnailFishNumber.explode

    val inputA: SnailFishNumber = SnailFishNumber.parsePairs("[[[[[9,8],1],2],3],4]")
    val expectedA: SnailFishNumber = SnailFishNumber.parsePairs("[[[[0,9],2],3],4]")
    assertEquals(explode(inputA), expectedA)

    val inputB: SnailFishNumber = SnailFishNumber.parsePairs("[7,[6,[5,[4,[3,2]]]]]")
    val expectedB: SnailFishNumber = SnailFishNumber.parsePairs("[7,[6,[5,[7,0]]]]")
    assertEquals(explode(inputB), expectedB)

    val inputC: SnailFishNumber = SnailFishNumber.parsePairs("[[6,[5,[4,[3,2]]]],1]")
    val expectedC: SnailFishNumber = SnailFishNumber.parsePairs("[[6,[5,[7,0]]],3]")
    assertEquals(explode(inputC), expectedC)

    val inputD: SnailFishNumber = SnailFishNumber.parsePairs("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")
    val expectedD: SnailFishNumber = SnailFishNumber.parsePairs("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
    assertEquals(explode(inputD), expectedD)

    val inputE: SnailFishNumber = SnailFishNumber.parsePairs("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
    val expectedE: SnailFishNumber = SnailFishNumber.parsePairs("[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
    assertEquals(explode(inputE), expectedE)


  }

  test("calculate the magnitude of multiple test cases provided") {
    val inputA = SnailFishNumber.parsePairs("[[1,2],[[3,4],5]]") //  becomes 143.
    val inputB = SnailFishNumber.parsePairs("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]") // becomes 1384.
    val inputC = SnailFishNumber.parsePairs("[[[[1,1],[2,2]],[3,3]],[4,4]]") //becomes 445.
    val inputD = SnailFishNumber.parsePairs("[[[[3,0],[5,3]],[4,4]],[5,5]]") // becomes 791.
    val inputE = SnailFishNumber.parsePairs("[[[[5,0],[7,4]],[5,5]],[6,6]]") // becomes 1137.
    val inputF = SnailFishNumber.parsePairs("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]") //becomes 3488.

    assertEquals(inputA.magnitude, 143L)
    assertEquals(inputB.magnitude, 1384L)
    assertEquals(inputC.magnitude, 445L)
    assertEquals(inputD.magnitude, 791L)
    assertEquals(inputE.magnitude, 1137L)
    assertEquals(inputF.magnitude, 3488L)

  }

  test("Calculate a larger example end to end reductions and then magnitude") {
    val parsedLargeInput = SnailFishNumber.parseInput(TEST_LARGE_HOMEWORK_ASSIGNMENT)
    val expectedReduction = SnailFishNumber.parsePairs("[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]")
    assertEquals(parsedLargeInput, expectedReduction)
    assertEquals(parsedLargeInput.magnitude, 4140L)
  }

  test("Find highest magnitude by combining all combinations of homework") {
    ???
  }

  // Helper method to quickly make test snailfishnumbers.
  def sfn(l: Int | SnailFishNumber, r: Int | SnailFishNumber): SnailFishNumber = SnailFishNumber(l, r)

}
