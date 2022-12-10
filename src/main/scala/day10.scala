import scala.annotation.tailrec

object day10 {
  case class Instruction(amount: Int, cycles: Int)

  def parseInput(inputStr: String): Array[Instruction] = {
    inputStr.split("\n")
      .map(line =>
        if line.substring(0, 4).equals("addx")
        then Instruction(line.substring(5).toInt, 2)
        else Instruction(0, 1)
      )
  }

  def getAddedValues(instruction: Instruction, currentValue: Int): Array[Int] = {
    1.until(instruction.cycles).map(_ => currentValue).toArray :+ (currentValue + instruction.amount)
  }

  @tailrec
  def getCycleValues(instructions: Array[Instruction], valuesSoFar: Array[Int] = Array(1)): Array[Int] = {
    instructions.length match
      case 0 => valuesSoFar
      case _ =>  getCycleValues(instructions.tail, valuesSoFar ++ getAddedValues(instructions.head, valuesSoFar.last))
  }

  def sumSignalStrengths(input: String): Int = {
    getCycleValues(parseInput(input)).zipWithIndex
      .map((value, idx) => (value, idx + 1))
      .filter((_, idx) => (idx - 20) % 40 == 0)
      .map((value, idx) => value * idx)
      .sum
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day10.txt")
    val input = try source.mkString finally source.close()
    println(s"Part 1 answer: ${sumSignalStrengths(input)}")
  }
}
