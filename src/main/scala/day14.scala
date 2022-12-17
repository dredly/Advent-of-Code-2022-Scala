import scala.annotation.tailrec
import scala.math.{max, min}

object day14 {
  sealed trait Material
  case object AIR extends Material
  case object ROCK extends Material
  case object SAND extends Material

  case class Point(x: Int, y: Int)
  case class Node(point: Point, material: Material = AIR)

  def drawLine(p1: Point, p2: Point): Set[Point] = {
    if p1.x == p2.x
    then min(p1.y, p2.y).to(max(p1.y, p2.y)).map(y => Point(p1.x, y)).toSet
    else min(p1.x, p2.x).to(max(p1.x, p2.x)).map(x => Point(x, p1.y)).toSet
  }

  def parseInputLine(line: String): Set[Point] = {
    line.split(" -> ").map(coordString => {
      val digits = coordString.split(",")
      Point(digits(0).toInt, digits(1).toInt)
    }).sliding(2, 1).map(points => drawLine(points(0), points(1)))
      .reduce((line1, line2) => line1.union(line2))
  }

  def parseInput(inputStr: String): Set[Point] = {
    inputStr.split("\n")
      .map(line => parseInputLine(line))
      .reduce((set1, set2) => set1.union(set2))
  }

  def makeGrid(lines: Set[Point]): Set[Node] = {
    val minX = lines.minBy(p => p.x).x
    val maxX = lines.maxBy(p => p.x).x
    val maxY = lines.maxBy(p => p.y).y
    val allPoints = 0.to(maxY).flatMap(y => minX.to(maxX).map(x => Point(x, y))).toSet
    val airPoints = allPoints.diff(lines)
    airPoints.map(p => Node(p)).union(lines.map(p => Node(p, ROCK)))
  }

  @tailrec
  def findPlacement(nodes: Set[Node], spawnLocation: Point): Option[Point] = {
    val below = nodes.find(n => n.point.x == spawnLocation.x & n.point.y == spawnLocation.y + 1)
    val belowLeft = nodes.find(n => n.point.x == spawnLocation.x - 1 & n.point.y == spawnLocation.y + 1)
    val belowRight = nodes.find(n => n.point.x == spawnLocation.x + 1 & n.point.y == spawnLocation.y + 1)
    below match
      case None => Option.empty
      case Some(below) =>
        below.material match
          case AIR => findPlacement(nodes, below.point)
          case _ =>
            belowLeft match
              case None => Option.empty
              case Some(belowLeft) =>
                belowLeft.material match
                  case AIR => findPlacement(nodes, belowLeft.point)
                  case _ =>
                    belowRight match
                      case None => Option.empty
                      case Some(belowRight) =>
                        belowRight.material match
                          case AIR => findPlacement(nodes, belowRight.point)
                          case _ => Option(spawnLocation)
  }

  @tailrec
  def placeSand(nodes: Set[Node], spawnLocation: Point): Set[Node] = {
    val placement = findPlacement(nodes, spawnLocation)
    placement match
      case None => nodes
      case Some(placement) =>
        val updatedNodes = nodes.map(n => if n.point == placement then n.copy(material = SAND) else n)
        placeSand(updatedNodes, spawnLocation)
  }

  def countSand(inputStr: String): Int = {
    placeSand(makeGrid(parseInput(inputStr)), Point(500, 0)).count(n => n.material == SAND)
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day14.txt")
    val input = try source.mkString finally source.close()
    println(s"Part 1 answer: ${countSand(input)}")
  }
}
