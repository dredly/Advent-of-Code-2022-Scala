import scala.annotation.tailrec

object day15 {
  case class Point(x: Int, y: Int)
  case class Sensor(sensorPoint: Point, beaconPoint: Point, distToNearestBeacon: Int)
  case class HorizontalRange(start: Int, end: Int)

  def manhattanDistance(p1: Point, p2: Point): Int = (p1.x - p2.x).abs + (p1.y - p2.y).abs

  def parseLine(line: String): Sensor = {
    val numbers = "[-0-9]+".r.findAllIn(line).map(digits => digits.toInt).toArray
    val sensorPoint = Point(numbers(0), numbers(1))
    val beaconPoint = Point(numbers(2), numbers(3))
    Sensor(sensorPoint, beaconPoint, manhattanDistance(sensorPoint, beaconPoint))
  }

  def parseLines(inputStr: String): Array[Sensor] = inputStr.split("\n").map(parseLine)

  def getLowestY(sensor: Sensor): Int = sensor.sensorPoint.y + sensor.distToNearestBeacon
  def getHighestY(sensor: Sensor): Int = sensor.sensorPoint.y - sensor.distToNearestBeacon

  def retainRelevantSensors(sensors: Array[Sensor], row: Int): Array[Sensor] =
    sensors.filter(s => getLowestY(s) >= row & getHighestY(s) <= row)

  def getNumOfBeaconsOnRow(sensors: Array[Sensor], row: Int): Int =
    sensors.filter(s => s.beaconPoint.y == row).distinctBy(s => s.beaconPoint.x).length

  def getHorizontalRange(sensor: Sensor, row: Int): HorizontalRange = {
    val triangleHeight = if sensor.sensorPoint.y < row then getLowestY(sensor) - row else row - getHighestY(sensor)
    HorizontalRange(sensor.sensorPoint.x - triangleHeight, sensor.sensorPoint.x + triangleHeight)
  }

  def overlapRanges(overlapped: Array[HorizontalRange], current: HorizontalRange): HorizontalRange = {
    val newStart = overlapped(0).start
    val newEnd = (overlapped :+ current).maxBy(hr => hr.end).end
    HorizontalRange(newStart, newEnd)
  }

  @tailrec
  def combineRanges(ranges: Array[HorizontalRange], idx: Int = 1): Array[HorizontalRange] = {
    if idx == ranges.length then ranges else {
      val (overlappedRanges, notOverlapped) = ranges.take(idx).partition(r => ranges(idx).start - r.end <= 1)

      val updatedPrevRanges = if overlappedRanges.isEmpty
      then ranges.take(idx + 1)
      else notOverlapped ++ Array(overlapRanges(overlappedRanges, ranges(idx)))

      val newIdx = idx + 1 - (ranges.take(idx + 1).length - updatedPrevRanges.length)
      val updatedRanges = updatedPrevRanges ++ ranges.drop(idx + 1)
      combineRanges(updatedRanges, newIdx)
    }
  }

  def getHorizontalRanges(sensors: Array[Sensor], row: Int): Array[HorizontalRange] =
    sensors.map(s => getHorizontalRange(s, row)).sortBy(hr => hr.start)

  def totalNonViablePositionsOnRow(inputStr: String, row: Int): Int = {
    val relevantSensors = retainRelevantSensors(parseLines(inputStr), row)
    val numHorizontalPositions = combineRanges(getHorizontalRanges(relevantSensors, row))
      .map(r => (r.end - r.start) + 1)
      .sum
    val numBeacons = getNumOfBeaconsOnRow(relevantSensors, row)
    numHorizontalPositions - numBeacons
  }

  @tailrec
  def findBeaconPosition(sensors: Array[Sensor], searchSpaceSize: Int, currentRow: Int = 0): Point = {
    if currentRow > searchSpaceSize then throw Exception("Did not find beacon") else {
      val relevantSensors = retainRelevantSensors(sensors, currentRow)
      val ranges = combineRanges(getHorizontalRanges(relevantSensors, currentRow))
      ranges.length match
        case 1 => findBeaconPosition(sensors, searchSpaceSize, currentRow + 1)
        case _ =>
          val x = ranges(0).end + 1
          Point(x, currentRow)
    }
  }

  def getTuningFrequency(inputStr: String, searchSpaceSize: Int): BigInt = {
    val beaconLocation = findBeaconPosition(parseLines(inputStr), searchSpaceSize)
    BigInt(beaconLocation.x) * BigInt(4_000_000) + BigInt(beaconLocation.y)
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day15.txt")
    val input = try source.mkString finally source.close()
    println(s"Part 1 answer: ${totalNonViablePositionsOnRow(input, 2_000_000)}")
    println(s"Part 2 answer: ${getTuningFrequency(input, 4_000_000)}")
  }
}
