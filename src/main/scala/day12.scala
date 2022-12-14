import scala.annotation.tailrec

object day12 {
  // Use instead of max value to avoid weird overflow issues
  val LARGE_NUMBER: Int = Integer.MAX_VALUE / 2

  case class Node(x: Int, y: Int, height: Int, distance: Int = LARGE_NUMBER, isStart: Boolean = false, isEnd: Boolean = false)

  def makeNode(character: Char, x: Int, y: Int): Node = {
    if character == 'S' then Node(x, y, 'a'.toInt, distance = 0, isStart = true) else {
      if character == 'E' then Node(x, y, 'z'.toInt, isEnd = true) else Node(x, y, character.toInt)
    }
  }

  def parseNodes(inputStr: String): Set[Node] = {
    inputStr.split("\n").zipWithIndex.flatMap { case (line, yIdx) =>
      line.zipWithIndex.map { case (chr, xIdx) => makeNode(chr, xIdx, yIdx) }.toArray
    }.toSet
  }

  def findNodeByCoords(x: Int, y: Int, nodes: Set[Node]): Option[Node] = nodes.find(node => node.x == x & node.y == y)

  def getNeighbours(nodes: Set[Node], x: Int, y: Int): Set[Node] = {
    Array((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
      .flatMap(coordPair => findNodeByCoords(coordPair(0), coordPair(1), nodes))
      .toSet
  }

  @tailrec
  def dijkstra(unexplored: Set[Node]): Int = {
    if unexplored.isEmpty then throw new Exception("Could not find path")
    val currentNode = unexplored.minBy(_.distance)
    if currentNode.isEnd then currentNode.distance else {
      val viableNeighbours = getNeighbours(unexplored, currentNode.x, currentNode.y)
        .filter(n => n.height - currentNode.height <= 1)
      val updatedNeighbours = viableNeighbours.map(n => n.copy(distance = currentNode.distance + 1))
      val updatedUnexplored = unexplored
        .filter(n => n != currentNode & !viableNeighbours.contains(n))
        .union(updatedNeighbours)
      dijkstra(updatedUnexplored)
    }
  }

  def getShortestPathLength(inputStr: String): Int = dijkstra(parseNodes(inputStr))

  def makeNodeBackwards(character: Char, x: Int, y: Int): Node = {
    if character == 'S' then Node(x, y, 'a'.toInt) else {
      if character == 'E' then Node(x, y, 'z'.toInt, distance = 0, isStart = true) else Node(x, y, character.toInt)
    }
  }

  def parseNodesBackwards(inputStr: String): Set[Node] = {
    inputStr.split("\n").zipWithIndex.flatMap { case (line, yIdx) =>
      line.zipWithIndex.map { case (chr, xIdx) => makeNodeBackwards(chr, xIdx, yIdx) }.toArray
    }.toSet
  }

  @tailrec
  def dijkstraTrackingExplored(unexplored: Set[Node], explored: Set[Node] = Set()): Set[Node] = {
    if unexplored.isEmpty then explored else {
      val currentNode = unexplored.minBy(_.distance)
      val viableNeighbours = getNeighbours(unexplored, currentNode.x, currentNode.y)
        .filter(n => currentNode.height - n.height <= 1)
      val updatedNeighbours = viableNeighbours.map(n => n.copy(distance = currentNode.distance + 1))
      val updatedUnexplored = unexplored
        .filter(n => n != currentNode & !viableNeighbours.contains(n))
        .union(updatedNeighbours)
      dijkstraTrackingExplored(updatedUnexplored, explored + currentNode)
    }
  }

  def getShortestPathLengthFromAnyStart(inputStr: String): Int = {
    dijkstraTrackingExplored(parseNodesBackwards(inputStr))
      .filter(n => n.height == 'a'.toInt)
      .minBy(_.distance)
      .distance
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day12.txt")
    val input = try source.mkString finally source.close()
    println(s"Part 1 answer: ${getShortestPathLength(input)}")
    println(s"Part 2 answer: ${getShortestPathLengthFromAnyStart(input)}")
  }
}
