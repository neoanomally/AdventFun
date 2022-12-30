package com.sandersme.advent.twentytwo.model

class FSCommandTest extends munit.FunSuite {
  test("Test that we can parse a cd command from parse commands to directory ..") {
    val testInput = List("$ cd ..")
    val (cd, remainingLines) = FSCommand.parseNextCommandLine(testInput)
    val expectedCd = CD("..")

    assertEquals(remainingLines.isEmpty, true)
    assertEquals(cd, expectedCd)
  }

  test("Test that we can parse the next ls command that has two directories and one file") {
    val testInput = List(
      "$ ls",
      "dir a",
      "dir b",
      "29116 f"
    )

    val (ls, remainingLines) = FSCommand.parseNextCommandLine(testInput)

    val expectedFiletypes: List[FileType] = List(
      Directory("a"),
      Directory("b"),
      File("f", 29116L)
    )

    val expectedLs = LS(expectedFiletypes)

    assertEquals(ls, expectedLs)
  }

  test("Validate that we can parse all command lines from start to finish") {
    val testInput = """$ cd /
                      |$ ls
                      |dir a
                      |14848514 b.txt
                      |8504156 c.dat
                      |dir d
                      |$ cd a
                      |$ ls
                      |dir e
                      |29116 f
                      |2557 g
                      |62596 h.lst
                      |$ cd e
                      |$ ls
                      |584 i
                      |$ cd ..
                      |$ cd ..
                      |$ cd d
                      |$ ls
                      |4060174 j
                      |8033020 d.log
                      |5626152 d.ext
                      |7214296 k""".stripMargin.split("\n").toList

    val allCommands = FSCommand.parseAllFSCommands(testInput)

    assertEquals(allCommands.length, 10)
  }
}
