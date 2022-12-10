import scala.annotation.tailrec

object day9 {
  sealed trait Direction
  case object UP extends Direction
  case object DOWN extends Direction
  case object LEFT extends Direction
  case object RIGHT extends Direction

  case class Point(x: Int, y: Int)
  case class Instruction(direction: Direction, distance: Int)

  def parseInput(inputStr: String): Array[Instruction] = {
    inputStr.split("\n").map(line => {
      line.substring(0, 1)  match
        case "U" => Instruction(UP, line.substring(2).toInt)
        case "D" => Instruction(DOWN, line.substring(2).toInt)
        case "L" => Instruction(LEFT, line.substring(2).toInt)
        case "R" => Instruction(RIGHT, line.substring(2).toInt)
    })
  }

  def moveHead(currentPosition: Point, instruction: Instruction): List[Point] = {
    instruction.direction match
      case UP =>
        currentPosition.y.until(currentPosition.y + instruction.distance)
          .map(y => Point(currentPosition.x, y + 1)).toList
      case DOWN =>
        currentPosition.y.until(currentPosition.y - instruction.distance, -1)
          .map(y => Point(currentPosition.x, y - 1)).toList
      case LEFT =>
        currentPosition.x.until(currentPosition.x - instruction.distance, -1)
          .map(x => Point(x -1, currentPosition.y)).toList
      case RIGHT =>
        currentPosition.x.until(currentPosition.x + instruction.distance)
          .map(x => Point(x + 1, currentPosition.y)).toList
  }

  def moveTail(currentPosition: Point, instruction: Instruction, headPosition: Point): List[Point] = {
    val cutCorner = (List(UP, DOWN).contains(instruction.direction) & currentPosition.x != headPosition.x & currentPosition.y == headPosition.y)
      | (List(LEFT, RIGHT).contains(instruction.direction) & currentPosition.y != headPosition.y & currentPosition.x == headPosition.x)
    val oppositeDirection = (instruction.direction == UP & headPosition.y < currentPosition.y)
      | (instruction.direction == DOWN & headPosition.y > currentPosition.y)
      | (instruction.direction == LEFT & headPosition.x > currentPosition.x)
      | (instruction.direction == RIGHT & headPosition.x < currentPosition.x)
    val diagonal = headPosition.x != currentPosition.x & headPosition.y != currentPosition.y
    if oppositeDirection & diagonal then moveHead(headPosition, instruction).init.drop(1) else {
      if currentPosition.equals(headPosition) | oppositeDirection then moveHead(headPosition, instruction).init else {
        if cutCorner then moveHead(headPosition, instruction).init else headPosition :: moveHead(headPosition, instruction).init
      }
    }
  }

  @tailrec
  def getVisitedPositions(
                           instructions: Array[Instruction],
                           visited: Set[Point] = Set(Point(0, 0)),
                           headPosition: Point = Point(0, 0),
                           tailPosition: Point = Point(0, 0)
                         ): Set[Point] = {
    instructions.length match
      case 0 => visited
      case _ =>
        getVisitedPositions(
          instructions.tail,
          visited.union(moveTail(tailPosition, instructions.head, headPosition).toSet),
          moveHead(headPosition, instructions.head).last,
          moveTail(tailPosition, instructions.head, headPosition).lastOption.getOrElse(tailPosition)
        )
  }

  def totalVisited(inputStr: String): Int = getVisitedPositions(parseInput(inputStr)).size

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day9.txt")
    val input = try source.mkString finally source.close()
    println(s"Part 1 answer: ${totalVisited(input)}")
  }
}
