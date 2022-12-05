object day3 {
  val OFFSET_LOWERCASE = 96
  val OFFSET_UPPERCASE = 38

  def getPriorities(inputArr: Array[String]): Array[Array[Int]] = {
    inputArr.map(line => line.toCharArray
      .map(ch => ch.toInt - (if (ch.isLower) OFFSET_LOWERCASE else OFFSET_UPPERCASE))
    )
  }

  def findCommonPriority(inputArrs: List[Array[Int]]): Int = {
    inputArrs.map(arr => arr.toSet).reduce((s1, s2) => s1.intersect(s2)).head
  }

  def getSumOfPriorities(inputArr: Array[String]): Int = {
    getPriorities(inputArr)
      .map(charArr => findCommonPriority(charArr.splitAt(charArr.length / 2).toList))
      .sum
  }

  def getSumOfBadgePriorities(inputArr: Array[String]): Int = {
    getPriorities(inputArr).sliding(3, 3)
      .map(groupOf3 => findCommonPriority(groupOf3.toList))
      .sum
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day3.txt")
    val input = try source.getLines().toArray finally source.close()
    println(s"Part 1 answer: ${getSumOfPriorities(input)}")
    println(s"Part 2 answer: ${getSumOfBadgePriorities(input)}")
  }
}
