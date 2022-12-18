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

  // Helper functions for debugging
  def printStats(valves: Set[Valve], bothGens: Array[Array[Valve]], timeLimit: Int): Unit = {
    val worstSolution = totalPressureRelease(valves, bothGens.last, timeLimit)
    println(s"Worst solution = $worstSolution")
  }

  // Evaluation function
  @tailrec
  def totalPressureRelease(allValves: Set[Valve], candidateValves: Array[Valve], timeLimit: Int, pressureReleaseSoFar: Int = 0): Int = {
    if candidateValves.length == 1 | timeLimit < 0 then pressureReleaseSoFar else {
      val (released: Int, minutesRemaining: Int) = eventualPressureRelease(allValves, candidateValves.head, candidateValves.tail.head, timeLimit)
      totalPressureRelease(allValves, candidateValves.tail, minutesRemaining, pressureReleaseSoFar + released)
    }
  }

  // Population initialization
  def generateCandidateSolution(allValves: Set[Valve], start: Valve): Array[Valve] = {
    val nonZeroFlowRateValves = allValves.filter(v => v.flowRate > 0).toList
    start +: util.Random.shuffle(nonZeroFlowRateValves).toArray
  }

  def generateCandidateSolutions(allValves: Set[Valve], start: Valve, numCandidates: Int): Array[Array[Valve]] = {
    0.until(numCandidates).map(_ => generateCandidateSolution(allValves, start)).toArray
  }

  // Parent Selection
  def selectParents(valves: Set[Valve], candidateSolutions: Array[Array[Valve]], timeLimit: Int): Array[Array[Valve]] = {
    // For now just using rank selection
    // TODO: Unhardcode the amount to take
    candidateSolutions.sortBy(candidate => -totalPressureRelease(valves, candidate, timeLimit)).take(10)
  }

  // Cloning
  def cloneParent(parent: Array[Valve]): Array[Valve] = {
    // Use probability to shuffle the latter parts of the array
    // The 0th index should never be shuffled as it is the given start point
    val arrLength = parent.length
    val probabilities = 0.until(arrLength)
    val randIdx = util.Random.between(1, arrLength)
    val startPoint = probabilities.find(p => p == randIdx).getOrElse(arrLength - 1)
    val (keepSame, toReshuffle) = parent.splitAt(startPoint)
    val reshuffled = util.Random.shuffle(toReshuffle.toList).toArray
    keepSame ++ reshuffled
  }

  def mutate(candidateSolution: Array[Valve]): Array[Valve] = {
    val swapWithIdx = util.Random.between(2, candidateSolution.length)
    candidateSolution.take(1) ++ Array(candidateSolution(swapWithIdx)) ++ candidateSolution.drop(2)
  }

  def progressGeneration(valves: Set[Valve], timeLimit: Int, candidateSolutions: Array[Array[Valve]]): Array[Array[Valve]] = {
    val parents = selectParents(valves, candidateSolutions, timeLimit)
    val clones = parents.flatMap(p => 0.until(20).map(_ => cloneParent(p)).toArray)
    val bothGens = (candidateSolutions ++ clones).sortBy(candidate => -totalPressureRelease(valves, candidate, timeLimit))
    val elites = bothGens.take(3)
    val normals = bothGens.drop(3).toList
    val shuffledNormals = util.Random.shuffle(normals).toArray.take(197)
    val newGen = elites ++ shuffledNormals
    println(s"Best solution value so far: ${totalPressureRelease(valves, newGen.head, timeLimit)}")
    printStats(valves, bothGens, timeLimit)
    newGen
  }

  @tailrec
  def evolve(valves: Set[Valve], timeLimit: Int, candidateSolutions: Array[Array[Valve]], generationsRemaining: Int): Int = {
    generationsRemaining match
      case 0 => totalPressureRelease(valves, candidateSolutions.head, timeLimit)
      case _ =>
        val newGen = progressGeneration(valves, timeLimit, candidateSolutions)
        evolve(valves, timeLimit, newGen, generationsRemaining - 1)
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day16.txt")
    val input = try source.mkString finally source.close()
    val valves = parseLines(input)
    val startValve = valves.find(v => v.uniqueName == "TU").get
    val candidates = generateCandidateSolutions(valves, startValve, 200)
    val finalGenValue = evolve(valves, 30, candidates, 200)
    println()
  }
}
