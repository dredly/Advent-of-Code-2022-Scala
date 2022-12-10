import scala.annotation.tailrec

object day10 {
  case class Instruction(amount: Int, cyclesRemaining: Int)

  def parseInput(inputStr: String): Array[Instruction] = {
    inputStr.split("\n")
      .map(line =>
        if line.substring(0, 4).equals("addx")
        then Instruction(line.substring(5).toInt, 2)
        else Instruction(0, 1)
      )
  }

  def updateValue(currentValue: Int, pendingInstructions: Array[Instruction]): Int = {
    pendingInstructions.length match
      case 0 => currentValue
      case _ => pendingInstructions
        .filter(pi => pi.cyclesRemaining == 0)
        .map(instruction => instruction.amount)
        .sum
        .+(currentValue)
  }

  def updatePendingInstructions(currentPending: Array[Instruction], newInstruction: Instruction): Array[Instruction] = {
    currentPending
      .filter(pi => pi.cyclesRemaining > 0)
      .map(pi => pi.copy(cyclesRemaining = pi.cyclesRemaining - 1))
      .:+(newInstruction)
  }

  @tailrec
  def getCycleValues(instructions: Array[Instruction], valuesSoFar: Array[Int] = Array(1), pendingInstructions: Array[Instruction] = Array()): Array[Int] = {
    instructions.length match
      case 0 => valuesSoFar
      case _ =>
        getCycleValues(
          instructions.tail,
          valuesSoFar :+ updateValue(valuesSoFar.last, pendingInstructions),
          updatePendingInstructions(pendingInstructions, instructions.head)
        )
  }

  def getValues(input: String): Array[Int] = {
    getCycleValues(parseInput(input))
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day10.txt")
    val input = try source.mkString finally source.close()
    println(getValues(input).mkString("Array(", ", ", ")"))
  }

}
