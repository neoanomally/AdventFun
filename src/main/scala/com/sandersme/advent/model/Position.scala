package com.sandersme.advent.model

case class Position(horizontal: Int, depth: Int, aim: Int) {
  def multiplyPosition = horizontal * depth
}

object Position {


  val EMPTY_POSITION = Position(0, 0, 0)

  extension (position: Position)
    def accumulate(pilotCommand: PilotCommand) = accumulatePilotCommand(position, pilotCommand)

  def accumulatePilotCommand(position: Position, pilotCommand: PilotCommand): Position = {
    pilotCommand.direction match {
      case Direction.Up => position.copy(aim = position.aim - pilotCommand.units)
      case Direction.Down => position.copy(aim = position.aim + pilotCommand.units)
      case Direction.Forward => {
        val depth = position.depth + position.aim * pilotCommand.units
        val horizontal = position.horizontal + pilotCommand.units

        Position(horizontal, depth, position.aim)
      }
      case Direction.Left => assert(false, "Should not make it here for pilot commands")
    }
  }

  def calculatePosition(pilotCommands: List[PilotCommand]): Position = {
    pilotCommands
      .foldLeft(EMPTY_POSITION){ (position, command) =>
        position.accumulate(command)
      }
  }
}
