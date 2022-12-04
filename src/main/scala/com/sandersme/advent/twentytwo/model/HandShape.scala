package com.sandersme.advent.twentytwo.model


enum HandShape {
  case Rock, Paper, Scissors
}

object HandShape {
  def handShapeFromEncoder(encodedMsg: String): HandShape = {
    encodedMsg match {
      case "A" | "X" => HandShape.Rock
      case "B" | "Y" => HandShape.Paper
      case "C" | "Z" => HandShape.Scissors
      case _ => throw new Exception("Error Parsing Input")
    }
  }

  def shapeScore(handShape: HandShape) = handShape match {
    case HandShape.Rock => 1
    case HandShape.Paper => 2
    case HandShape.Scissors => 3
  }
}