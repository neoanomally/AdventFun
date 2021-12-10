package com.sandersme.advent.model

case class Segment(values: Set[Char])

val CORE_SEGMENTS: Map[Segment, Int] = Map(
  Segment(Set('a', 'b', 'c', 'e', 'f', 'g'))      -> 0, // 6
  Segment(Set('c', 'f'))                          -> 1, // 2
  Segment(Set('a', 'c', 'd', 'e', 'g'))           -> 2, // 5
  Segment(Set('a', 'c','d', 'f', 'g'))            -> 3, // 5
  Segment(Set('b', 'c', 'd', 'f'))                -> 4, // 4
  Segment(Set('a', 'b', 'd', 'f', 'g'))           -> 5, // 5
  Segment(Set('a', 'b', 'd', 'e', 'f', 'g'))      -> 6, // 6
  Segment(Set('a', 'c', 'f'))                     -> 7, // 3
  Segment(Set('a', 'b', 'c', 'd', 'e', 'f', 'g')) -> 8, // 7
  Segment(Set('a', 'b', 'c', 'd', 'f', 'g'))      -> 9  // 6
)