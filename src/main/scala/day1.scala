object day1 {
  def getMostCalories(inputStr: String): Int = inputStr.split("\n\n")
    .map(elf => elf.split("\n")
      .map(cals => cals.toInt)
      .sum)
    .max

  def getTopThreeCaloriesTotal(inputStr: String): Int = inputStr.split("\n\n")
    .map(elf => elf.split("\n")
      .map(cals => cals.toInt)
      .sum)
    .sorted(Ordering.Int.reverse).take(3).sum

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day1.txt")
    val input = try source.mkString finally source.close()
    println(s"Part 1 answer: ${getMostCalories(input)}")
    println(s"Part 2 answer: ${getTopThreeCaloriesTotal(input)}")
  }
}
