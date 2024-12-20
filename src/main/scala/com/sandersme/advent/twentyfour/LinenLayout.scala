package com.sandersme.advent.twentyfour

import scala.annotation.tailrec
import scala.collection.mutable.{Set => MSet}
import scala.collection.mutable.{Map => MMap}
import com.sandersme.advent.Input
import com.sandersme.advent.twentythree.CosmicExpansion.input
import com.sandersme.advent.twentyfour.PrefixTrie
import scala.runtime.stdLibPatches.language.noAutoTupling

/** do we want to structure the patterns in a certain way? I might actually want to have
 *  the patterns as a trie **/ 
// TODO: Maybe I keep a cache of longer patterns
case class LinenLayout(patterns: PrefixTrie, linens: List[String], longestPattern: Int, debug: Boolean) {

  // What we can do is a recursive check
  // at idx, 0 -> 
  @tailrec
  final def isValidPattern(linen: String, length: Int = 1): Boolean = {
    val isValid = recursionValid(linen, 0, length)
    if (isValid) {
      true 
    } else if (length > longestPattern || length >= linen.length) {
      false 
    } else {
      isValidPattern(linen, length + 1)
    }
  }

  @tailrec
  private def recursionValid(linen: String, startIdx: Int, length: Int): Boolean = {
    // println(f"StartIdx: $startIdx   length: $length   totalSize: ${linen.length}")
    val endIdx = startIdx + length
    if (startIdx >= linen.length) {
      true 
    } else if (length > longestPattern || endIdx > linen.length) {
      false 
    } else if(patterns.contains(linen.substring(startIdx, endIdx))) {
      recursionValid(linen, startIdx + length, 1)
    } else {
      recursionValid(linen, startIdx, length + 1)
    }
  }

  def countCombinationsOfPatterns: Long = {
    linens.map(l => (l, patterns.allSubWordsValid(l)))
      .filter(_._2)
      .map((linen, _) => patterns.startCountNumOptions(linen))
      .sum
  }

  def patternExists(in: String, leftIdx: Int, length: Int): Boolean = {
    if (debug) {
      println(in.substring(leftIdx, leftIdx + length))
    }
    patterns.contains(in.substring(leftIdx, leftIdx + length))
  }

  def countNumValidLinens: Int = {
    val validPatterns = linens.map(l => (l, patterns.allSubWordsValid(l)))
    
    // println("THese are the impossible patterns: ") 
    // validPatterns.filter(v => !v._2)
    //   .foreach(println)

    validPatterns.count(_._2)
  }
}

object PrefixTrie {
  def apply(in: Iterable[String]): PrefixTrie = {
    val prefixTrie = new PrefixTrie()

    in.foldLeft(prefixTrie)((agg, next) => agg.add(next))
  }

  def apply(): PrefixTrie = {
    new PrefixTrie()
  } 
} 

class PrefixTrie { 
  val head = Node(false)

  class Node (var isTerminal: Boolean) {
    val children: MMap[Char, Node] = MMap[Char, Node]()

    def setTerminal(isTerminal: Boolean): Node = {
      this.isTerminal = this.isTerminal || isTerminal
      this
    }

    override def toString(): String = "isTerminal: " + isTerminal + " - Children (" + children + ")"
  }

  def add(in: String): PrefixTrie = {
    add(in, 0, head)
  }

  @tailrec
  private def add(in: String, idx: Int = 0, node: Node): PrefixTrie = {
    if (idx == in.length - 1) {
      val updated = node.children.getOrElseUpdate(in(idx), new Node(false)).setTerminal(true)
      node.children + (in(idx) -> updated)
      this
    } else {
      val nodeToTravel = node.children.getOrElseUpdate(in(idx), Node(false)) 
      add(in, idx + 1, nodeToTravel)
    }
  }

  def contains(in: String): Boolean = {
    contains(in, 0, head)
  }

  @tailrec
  private def contains(in: String, idx: Int, node: Node): Boolean = {
    if (idx == in.length) {
      node.isTerminal
    } else if(node.children.contains(in(idx))) {
      contains(in, idx + 1, node.children(in(idx)))
    } else {
      false 
    }
  }

  def startCountNumOptions(in: String): Long = {
    val memo = iterativeCountNumOptions(in)
    memo(0)
  }


  /// Some dynamic programming to create a map starting backwards at the end of the list. 
  // I'm making zero copies of strings.
  def iterativeCountNumOptions(in: String): Map[Int, Long] = {
    in.zipWithIndex.foldRight(Map.empty[Int, Long]) { case ((letter, startIdx), startingMemoAgg) => 
      val allTerminalIndicies = findAllTerminalIndicies(in, startIdx)
      // println(startingMemoAgg)
      val currIdxAgg = allTerminalIndicies
        .foldRight(0L){case (endIndex, rollingAgg) =>  
       
          val defaultRes = if (endIndex == in.length) { 1L } else { 0L }
          val withMemoAfter = startingMemoAgg.getOrElse(endIndex , 0L)
          
          // println("\tEndIndex " + endIndex + f"   defaultRes ${defaultRes}")
          rollingAgg + withMemoAfter + defaultRes
        }
      // print(f"Current idx: ${startIdx} agg: ${currIdxAgg} MEMO: ")
        
      startingMemoAgg + (startIdx -> currIdxAgg)
        // this is where we update the memo
    }
  }

  def findAllTerminalIndicies(in: String, startIdx: Int): List[Int] = {
    findAllTerminalIndicies(in, List.empty, startIdx, startIdx, head) 
  }

  @tailrec
  private def findAllTerminalIndicies(in: String, res: List[Int], startIdx: Int, idx: Int, node: Node): List[Int] = {
    val updatedRes = if (node.isTerminal) { res :+ idx } else { res }
    if (idx == in.length) {
      updatedRes
    } else if (node.children.contains(in(idx))) {
      findAllTerminalIndicies(in, updatedRes, startIdx, idx + 1, node.children(in(idx)))
    } else {
      updatedRes
    } 
  }


  // idx 0 I have head which has children
  // I can see if idx by node.children.contains(in(end))
  /// If the start is above the length of the string we are finsihed and should return the
  //full total.. If the end is above string we should return totla + 1 
  // then I need to check if the end is terminal, if it is I can see if the next starting
  // index is memoized if so return that count; else  I can move up the start index to see
  // how many times the word ends + the number of times a word keeps going. If it's not 
  private def countAllSubWords(in: String, start: Int, end: Int, node: Node, memo: MMap[Int, Long]): Long = {
    // TODO Maybe we get here if the end == in.length && node.isTerminal as in we finished
    // the last word in the sequence
    // r, wr, b, g, bwu, rb, gb, br
    // 0, 0, 
    // rrbgbr
    // 0, 1 r rbgbr
    // 1, 1 
    // 1, 2 r r<- bgbr
    // 3, // start
    lazy val childContains = node.children.contains(in(end))
    lazy val child = node.children(in(end))
    if (start == in.length || (node.isTerminal && end == in.length)) {
      1L
    } else if (start == in.length || end == in.length) {
      0L
      // When I'm at node.isTerminal word index is actually start -> end - 1
    } else if (node.isTerminal && memo.contains(end + 1)) {
      memo(end + 1)
    } else if (node.isTerminal && !childContains) {
      val res = countAllSubWords(in, end, end, head, memo) 
      memo += (end -> res)
      memo(end)
    } else if (node.isTerminal && childContains) {
      val resA = countAllSubWords(in, end, end, head, memo)  
      val resB = countAllSubWords(in, start, end + 1, child, memo)
      memo += (end -> (resA + resB))
      memo(end)
    } else if (childContains) {  
      countAllSubWords(in, start, end + 1, child, memo)
    } else {
      0L
    } 
  }


  // @tailrec
  // private def findAllSubwordIdx(in: String,  res: List[Int], start: Int, endIdx: Int, node: Node, total): (List[Int], Int) = {
  //   val updatedRes = if (node.isTerminal) { res :+ endIdx + 1} else { res }
  //   if (start == in.length) {
  //     (List.empty, 1)
  //   } else if (node.children.contains(in(endIdx))) {
  //     findAllSubwordIdx(in, updatedRes, start, endIdx + 1, node.children(in(endIdx)), total)
  //   } else {
  //     (updatedRes, total)
  //   }
  // }


  def findAllSubwords(in: String): List[String] = {
    findAllSubwords(in, List.empty, 0, head) 
  }

  @tailrec
  private def findAllSubwords(in: String, res: List[String], idx: Int, node: Node): List[String] = {
    val updatedRes = if (node.isTerminal) { res :+ in.substring(0, idx ) } else { res }
    if (idx == in.length) {
      updatedRes
    } else if (node.children.contains(in(idx))) {
      findAllSubwords(in, updatedRes, idx + 1, node.children(in(idx)))
    } else {
      updatedRes
    } 
  }
  
  def allSubWordsValid(in: String): Boolean = {
    startAllSubwordsValid(in, 0)
  }


  private def startAllSubwordsValid(in: String, currWordIdx: Int): Boolean = {
    if (in.length == currWordIdx) {
      true 
    } else {
      allSubwordsValid(in, currWordIdx, currWordIdx, head)
    }
  }

  private def allSubwordsValid(in: String, currWordIdx: Int, currLetterIdx: Int, node: Node): Boolean = {
    lazy val childNode = node.children.get(in(currLetterIdx))

    if (currWordIdx == in.length) {
      true
    } else if (currLetterIdx >= in.length) {
      false
    } else if (childNode.map(_.isTerminal).getOrElse(false) && startAllSubwordsValid(in, currLetterIdx + 1)) {
      true
    } else if (childNode.map(_.isTerminal).getOrElse(false)) {
      allSubwordsValid(in, currWordIdx, currLetterIdx + 1, childNode.get)
    } else if(childNode.nonEmpty) {
      allSubwordsValid(in, currWordIdx, currLetterIdx + 1, childNode.get)
    } else {
      false
    }
  }


  override def toString(): String = head.toString()
}


object LinenLayout {
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyFourFromResource("day19_input")

    val linenLayout = LinenLayout.parseInput(input)
    val startTimePartOne = System.currentTimeMillis()
    val numValidPatterns = linenLayout.countNumValidLinens

    val endTimePartOne = System.currentTimeMillis
    val startTimePartTwo = System.currentTimeMillis()
    val countNumberCombinations = linenLayout.countCombinationsOfPatterns
    val endTimePartTwo = System.currentTimeMillis()
    val totalTimePartOne = endTimePartOne - startTimePartOne
    val totalTimePartTwo = endTimePartTwo - startTimePartTwo

    println(f"The number of valid linen patterns: $numValidPatterns   it took $totalTimePartOne ms")
    println(f"The number of combination of patterns: ${countNumberCombinations}    it took $totalTimePartTwo ms")
  }



  def parseInput(in: List[String]): LinenLayout = {
    val patterns = in.head.split(", ")
    val trie = PrefixTrie(patterns.toList)
    
    val linens = in.drop(2)
    val longestPattern = patterns.map(_.length).max 

    LinenLayout(trie, linens, longestPattern, false)
  }
}
