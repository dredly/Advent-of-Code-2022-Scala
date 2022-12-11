import day9.{DOWN, Direction, Instruction, LEFT, Point, RIGHT, UP, parseInput}
import scala.annotation.tailrec

object day9part2 {

  case class Move(from: Point, to: Point)

  def moveHead(startingPosition: Point, direction: Direction): Move = {
    direction match
      case UP => Move(startingPosition, startingPosition.copy(y = startingPosition.y + 1))
      case DOWN => Move(startingPosition, startingPosition.copy(y = startingPosition.y - 1))
      case LEFT => Move(startingPosition, startingPosition.copy(x = startingPosition.x - 1))
      case RIGHT => Move(startingPosition, startingPosition.copy(x = startingPosition.x + 1))
  }

  def takeAvgPlace(start: Point, end: Point): Point = {
    Point(x = (start.x + end.x) / 2, y = (start.y + end.y) / 2)
  }

  def isDirectlyNeighbouring(point1: Point, point2: Point): Boolean = {
    ((point1.x - point2.x).abs == 1 & point1.y == point2.y) | ((point1.y - point2.y).abs == 1 & point1.x == point2.x)
  }

  def isDiagonallyNeighbouring(point1: Point, point2: Point): Boolean = {
    (point1.x - point2.x).abs == 1 & (point1.y - point2.y).abs == 1
  }

  def isNeighbouring(point1: Point, point2: Point): Boolean = {
    isDirectlyNeighbouring(point1, point2) | isDiagonallyNeighbouring(point1, point2)
  }

  def moveTail(headMove: Move, tailPosition: Point): Move = {
    val onTop = headMove.from == tailPosition | headMove.to == tailPosition
    val didHeadMove = headMove.to != headMove.from
    val noTailMovement = onTop | !didHeadMove | isNeighbouring(headMove.to, tailPosition)

    val isAveragable = (headMove.to.x - tailPosition.x).abs % 2 == 0 & (headMove.to.y - tailPosition.y).abs % 2 == 0

    val followHead = isDiagonallyNeighbouring(headMove.from, tailPosition) & isDirectlyNeighbouring(headMove.from, headMove.to)

    if noTailMovement then Move(tailPosition, tailPosition) else {
      if isAveragable then Move(tailPosition, takeAvgPlace(tailPosition, headMove.to)) else {
        if followHead then Move(tailPosition, headMove.from) else {
          Move(tailPosition, tailPosition.copy(
            x = tailPosition.x + (headMove.to.x - headMove.from.x),
            y = tailPosition.y + (headMove.to.y - headMove.from.y)
          ))
        }
      }
    }
  }

  @tailrec
  def moveSnakeBody(headMove: Move, body: List[Point], direction: Direction, moves: List[Move] = List()): List[Point] = {
    body.length match
      case 0 => moves.map(move => move.to)
      case _ =>
        val newHeadMove = moveTail(headMove, body.head)
        moveSnakeBody(newHeadMove, body.tail, direction, moves :+ newHeadMove)
  }

  def moveWholeSnake(snake: List[Point], direction: Direction): List[Point] = {
    val headMove = moveHead(snake.head, direction)
    val movedBody = moveSnakeBody(headMove, snake.tail, direction)
    headMove.to::movedBody
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
        getVisititedPositionsForTail(instructions.tail, instructionResults(1), visitedSoFar.union(instructionResults(0)))
  }

  def totalVisitedByTail(inputStr: String, snakeLength: Int): Int = {
    val startingPositions = 0.until(snakeLength).map(_ => Point(0, 0)).toList
    getVisititedPositionsForTail(parseInput(inputStr), startingPositions).size
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day9.txt")
    val input = try source.mkString finally source.close()
    println(s"Part 2 answer: ${totalVisitedByTail(input, 10)}")
  }
}
