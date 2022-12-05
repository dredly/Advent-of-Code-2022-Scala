object day3 {
  val OFFSET_LOWERCASE = 96
  val OFFSET_UPPERCASE = 38

  def getSumOfPriorities(inputArr: Array[String]): Int = {
    inputArr.map(line => line.toCharArray
      .map(ch => ch.toInt - (if(ch.isLower) OFFSET_LOWERCASE else  OFFSET_UPPERCASE))
    ).map(charArr => charArr.splitAt(charArr.length / 2).toList
      .map(arr => arr.toSet)
      .reduce((s1, s2) => s1.intersect(s2)).head
    ).sum
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day3.txt")
    val input = try source.getLines().toArray finally source.close()
    println(s"Part 1 answer: ${getSumOfPriorities(input)}")
  }
}
