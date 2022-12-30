package com.sandersme.advent.twentytwo.model

class FileTypeTest extends munit.FunSuite {
  test("Parsing directory filetype Parsing is correct") {
    val directoryTestInput = "dir b"

    val filetype = FileType.parseFiletype(directoryTestInput)
    val expectedFiletype = Directory("b")

    assertEquals(filetype, expectedFiletype)
  }

  test("parsing file filetype parsing is correct") {
    val fileTestInput = "29116 f"

    val filetype = FileType.parseFiletype(fileTestInput)
    val expectedFiletype = File("f", 29116L)

    assertEquals(filetype, expectedFiletype)
  }
}
