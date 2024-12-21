package com.sandersme.advent.twentythree.model

import com.sandersme.advent.twentythree.model.Diagnostic
import com.sandersme.advent.twentythree.model.Diagnostic._
import scala.annotation.tailrec


enum Diagnostic {
  case Working, Broken, Unknown 
}

case class HotSpringsDiagnostics(diagnostics: List[Diagnostic], damagedGroups: List[Int]) {
  def allPossibleDiagnostics: List[List[Diagnostic]] = {
    HotSpringsDiagnostics.branchingDiagnostics(diagnostics)
  }

  /**
    * Method finds all possible diagnostics then counts only the ones that match the 
    * contiguous broken.
    *
    * TODO pass along the count Contiguous broken with it. 
    *
    * @return
    */
  def countValidBranches: Int = {
    HotSpringsDiagnostics
      .branchingDiagnostics(diagnostics)
      .map(HotSpringsDiagnostics.countContiguousBroken)
      .count(_ == damagedGroups)
  }

  def unfold: HotSpringsDiagnostics = {
    val withUnknown = diagnostics :+ Diagnostic.Unknown
    val updatedDiagnostics = withUnknown ++ withUnknown ++ withUnknown ++ withUnknown ++ diagnostics
    val groups = damagedGroups ++ damagedGroups ++ damagedGroups ++ damagedGroups ++ damagedGroups

    HotSpringsDiagnostics(updatedDiagnostics, groups)
  }

  /**
   * TODO This method will eventually call down to the @see BranchState
    * We need to go through each part one at a time and create a temporary fork. Check to
    * see if the fork matches the current count position. 
    *
    * @return
    */
  def countForkingBranches: Int = {
    1
  }
}


/**
  * TODO: Finish writing out this calss. The state is used to check if it's a valid state;
  * It contains the previous path so that we can build up the current state. We may need
  * to pass along additional information. 
  *
  * @param previous
  * @param currentContigDiagnostic
  */
case class BranchState(previous: List[Diagnostic], currentContigDiagnostic: Int) {
  def isValid: Boolean = ???
  def nextWithDiagnostic(diagnostic: Diagnostic): BranchState = {
    diagnostic match { 
      case Broken => BranchState(previous :+ diagnostic, currentContigDiagnostic + 1)
      case Working if currentContigDiagnostic == 0 => this.copy(previous :+ diagnostic)
      case Working => ??? 
      case Unknown => ???
    }
  }
}

object  HotSpringsDiagnostics {
  def parseInput(input: List[String]): List[HotSpringsDiagnostics] = {
    input
      .map(parseSingleInput)
  }

  def parseSingleInput(input: String): HotSpringsDiagnostics = {
    val diagnosticGroupSplit = input.split(" ")

    val diagnostics = diagnosticGroupSplit(0)
      .map {
        case '.' => Diagnostic.Working
        case '#' => Diagnostic.Broken
        case '?' => Diagnostic.Unknown
        case n@_   => throw new Exception(s"Error parsing diagnostic: ${n} - Not Valid.")  
      }.toList

    val damagedGroups = diagnosticGroupSplit(1).split(",")
      .map(_.toInt)
      .toList

    HotSpringsDiagnostics(diagnostics, damagedGroups)
  }
 
  /**
    * This method counts all possible arrangements across the diagnostics
    *
    * @param diagnostics
    * @return
    */
  def countAllPossibleArrangements(diagnostics: List[HotSpringsDiagnostics]): Int = {
    diagnostics
      .map(_.countValidBranches)
      .sum
  }


  // Takes in a group of diagnostics and calculates the set of contiguous groups of
  // broken records. Any time there is a working record that's essentially a comma. Every
  // time we find a comma that's a forking branch in possible future state paths. 
  // TODO: We can filter WHILE building these. This is important to reduce the number 
  // of possible universes ahead of time. to do preemptive counts. This is an
  // optimization though
  def branchingDiagnostics(diagnostics: List[Diagnostic]): List[List[Diagnostic]] = {
    @tailrec
    def loop(remaining: List[Diagnostic], allBranches: List[List[Diagnostic]]): List[List[Diagnostic]] = {
      lazy val next = remaining.head
      lazy val left = remaining.tail

      if (remaining.isEmpty) {
        allBranches
      } else if (next == Diagnostic.Unknown) {
        // might have a condition where builtBranches is Empty 
        val updatedBranches = allBranches
          .flatMap{branch => 
            List(branch :+ Diagnostic.Broken, branch :+ Diagnostic.Working)
          }
        loop(left, updatedBranches)
      } else {
        val updatedBranches = allBranches.map(branch => branch :+ next)

        loop(left, updatedBranches)
      }
    }

    val bootstrap = if (diagnostics.head == Diagnostic.Unknown) {
      List(List(Diagnostic.Working), List(Diagnostic.Broken))
    } else {
      List(List(diagnostics.head))
    }

    loop(diagnostics.tail, bootstrap)
  } 

  private[this] def preEmptiveFilter(diagnostic: List[Diagnostic], filter: List[Int]): Boolean = {
    val contiguousCounts = countContiguousBroken(diagnostic)
 
    val torso = contiguousCounts.dropRight(1)
    val torsoFilter = filter.take(torso.size - 1)

    if (contiguousCounts.isEmpty) {
      true
    } else if (contiguousCounts.size > filter.size) {
      false
    } else if (torso != torsoFilter) {
      false
    } else {
      contiguousCounts.last <= filter(contiguousCounts.size - 1)
    }
  }

  case class CountingState(counts: List[Int], current: Int) {
    def finalCounts: List[Int] = {
      if (current > 0) {
        counts :+ current
      } else {
        counts 
      }
    }

    def nextWithDiagnostic(diagnostic: Diagnostic): CountingState = {
      diagnostic match {
        case Diagnostic.Working if current > 0 => 
          CountingState(counts :+ current, 0)
        case Diagnostic.Broken => this.copy(current = current + 1)
        case _ => this
      }
    }
  }

  object CountingState {
    def empty: CountingState = CountingState(List.empty, 0)
  }

  def countContiguousBroken(diagnostics: List[Diagnostic]): List[Int] = {
    @tailrec
    def loop(remaining: List[Diagnostic], state: CountingState): List[Int] = {
      if (remaining.isEmpty) {
        state.finalCounts
      } else {
        val next = remaining.head
        val left = remaining.tail

        loop(left, state.nextWithDiagnostic(next))
      }
    }

    loop(diagnostics, CountingState.empty)
  }
}

case class DiagnosticNode(working: Option[Boolean], broken: Option[Boolean])
case class riagnosticNodes(nodes: List[DiagnosticNode])
