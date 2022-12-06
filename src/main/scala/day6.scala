import scala.annotation.tailrec

object day6 {
  def allUniqueChars(str: String): Boolean = {
    str.toCharArray.distinct.length == str.length
  }

  @tailrec
  def getCharsProcessed(input: String, removed: Int, markerSize: Int): Int = {
    if input.length < markerSize then throw new Exception("No packet found")
    if allUniqueChars(input.substring(0, markerSize))
      then removed + markerSize
      else getCharsProcessed(input.substring(1), removed + 1, markerSize)
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day6.txt")
    val input = try source.mkString finally source.close()
    println(s"Part 1 answer: ${getCharsProcessed(input, 0, 4)}")
    println(s"Part 2 answer: ${getCharsProcessed(input, 0, 14)}")
  }
}
