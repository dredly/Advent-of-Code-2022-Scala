import day9.{DOWN, Direction, Instruction, LEFT, Point, RIGHT, UP, parseInput}

import scala.annotation.tailrec

object day9part2 {
  def moveHead(startingPosition: Point, direction: Direction): Point = {
    direction match
      case UP => startingPosition.copy(y = startingPosition.y + 1)
      case DOWN => startingPosition.copy(y = startingPosition.y - 1)
      case LEFT => startingPosition.copy(x = startingPosition.x - 1)
      case RIGHT => startingPosition.copy(x = startingPosition.x + 1)
  }

  def moveTail(headPosition: Point, tailPosition: Point, direction: Direction): Point = {
    val sameDirection = (direction == UP & headPosition.y > tailPosition.y)
      | (direction == DOWN & headPosition.y < tailPosition.y)
      | (direction == LEFT & headPosition.x < tailPosition.x)
      | (direction == RIGHT & headPosition.x > tailPosition.x)

    if sameDirection then headPosition else tailPosition
  }

  @tailrec
  def moveSnakeBody(head: Point, body: List[Point], direction: Direction, moved: List[Point] = List()): List[Point] = {
    body.length match
      case 0 => moved
      case _ =>
        val newHeadPosition = moveTail(head, body.head, direction)
        moveSnakeBody(body.head, body.tail, direction, moved :+ newHeadPosition)
  }

  def moveWholeSnake(snake: List[Point], direction: Direction): List[Point] = {
    val movedBody = moveSnakeBody(snake.head, snake.tail, direction)
    moveHead(snake.head, direction)::movedBody
  }

  @tailrec
  def doInstruction(instruction: Instruction, tailVisited: Set[Point], currentPositions: List[Point]): (Set[Point], List[Point]) = {
    instruction.distance match
      case 0 => (tailVisited, currentPositions)
      case _ =>
        val newSnakePositions = moveWholeSnake(currentPositions, instruction.direction)
        doInstruction(instruction.copy(distance = instruction.distance - 1), tailVisited + newSnakePositions.last, newSnakePositions)
  }

  @tailrec
  def getVisititedPositionsForTail(instructions: Array[Instruction], currentPositions: List[Point], visitedSoFar: Set[Point] = Set(Point(0, 0))): Set[Point] = {
    instructions.length match
      case 0 => visitedSoFar
      case _ =>
        val instructionResults = doInstruction(instructions.head, visitedSoFar, currentPositions)
        println(instructionResults)
        println()
        getVisititedPositionsForTail(instructions.tail, instructionResults(1), visitedSoFar.union(instructionResults(0)))
  }

  def totalVisitedByTail(inputStr: String, snakeLength: Int): Int = {
    val startingPositions = 0.until(snakeLength).map(_ => Point(0, 0)).toList
    getVisititedPositionsForTail(parseInput(inputStr), startingPositions).size
  }

  def main(args: Array[String]): Unit = {
//    val points = List(Point(3, 0), Point(2, 0), Point(1, 0), Point(0, 0))
//    val result = doInstruction(Instruction(RIGHT, 4), Set(Point(0, 0)), points)
//    val result = moveWholeSnake(points, RIGHT)
//    val result = moveTail(Point(1, 0), Point(0, 0), RIGHT)
//    println(result)
    val source = scala.io.Source.fromFile("day9.txt")
    val input = try source.mkString finally source.close()
    println(s"Part 2 answer: ${totalVisitedByTail(input, 10)}")
  }
}
