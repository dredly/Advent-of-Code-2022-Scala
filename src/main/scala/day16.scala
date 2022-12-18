import scala.annotation.tailrec

object day16 {
  val LARGE_NUMBER: Int = Integer.MAX_VALUE / 2

  case class Valve(uniqueName: String, flowRate: Int, tunnelsTo: Array[String], distance: Int = LARGE_NUMBER)

  def parseLine(line: String): Valve = {
    val uniqueNames = raw"[A-Z]{2}".r.findAllIn(line).toArray
    val flowRate = raw"\d+".r.findFirstIn(line).get.toInt
    Valve(uniqueNames.head, flowRate, uniqueNames.tail)
  }

  def parseLines(inputStr: String): Set[Valve] = inputStr.split("\n").map(parseLine).toSet

  @tailrec
  def dijkstra(unexplored: Set[Valve], endValveName: String): Int = {
    if unexplored.isEmpty then throw new Exception("Could not find path") else {
      val currentValve = unexplored.minBy(_.distance)
      if currentValve.uniqueName == endValveName then currentValve.distance else {
        val neighbours = unexplored.filter(v => currentValve.tunnelsTo.contains(v.uniqueName))
        val updatedNeighbours = neighbours.map(n => n.copy(distance = currentValve.distance + 1))
        val updatedUnexplored = unexplored
          .filter(n => n != currentValve & !neighbours.contains(n))
          .union(updatedNeighbours)
        dijkstra(updatedUnexplored, endValveName)
      }
    }
  }

  def eventualPressureRelease(valves: Set[Valve], currentValve: Valve, candidateValve: Valve, minutesRemaining: Int): (Int, Int) = {
    val startValve = currentValve.copy(distance = 0)
    val pathFindingValves = (valves - currentValve) + startValve
    val distanceBetween = dijkstra(pathFindingValves, candidateValve.uniqueName)
    // Add 1 to account for the time take opening the valve
    val newMinutesRemaining = minutesRemaining - (distanceBetween + 1)
    (newMinutesRemaining * candidateValve.flowRate, newMinutesRemaining)
  }

  // Evaluation function
  @tailrec
  def totalPressureRelease(allValves: Set[Valve], candidateValves: Array[Valve], timeLimit: Int, pressureReleaseSoFar: Int = 0): Int = {
    if candidateValves.length == 1 | timeLimit < 0 then pressureReleaseSoFar else {
      val (released: Int, minutesRemaining: Int) = eventualPressureRelease(allValves, candidateValves.head, candidateValves.tail.head, timeLimit)
      totalPressureRelease(allValves, candidateValves.tail, minutesRemaining, pressureReleaseSoFar + released)
    }
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day16.txt")
    val input = try source.mkString finally source.close()
    val valves = parseLines(input)
    val proposedSolution = Array("AA", "DD", "BB", "JJ", "HH", "EE", "CC").map(name => valves.find(v => v.uniqueName == name).get)
    val totalPressureReleased = totalPressureRelease(valves, proposedSolution, 30)
    println()
  }
}
