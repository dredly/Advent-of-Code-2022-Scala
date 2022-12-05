object day4 {
  def setFromStringRepr(stringRepr: String): Set[Int] = {
    val startAndEnd = stringRepr.split("-").map(digits => digits.toInt)
    startAndEnd(0).to(startAndEnd(1)).toSet
  }

  def findAssignmentPairsWithOverlappingRanges(inputArr: Array[String]): Int = {
    inputArr.map(line => line.split(",").map(s => setFromStringRepr(s)))
      .count(arr => arr(0).subsetOf(arr(1)) | arr(1).subsetOf(arr(0)))
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day4.txt")
    val input = try source.getLines().toArray finally source.close()
    println(s"Part 1 answer: ${findAssignmentPairsWithOverlappingRanges(input)}")
  }
}
