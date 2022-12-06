import day1.{getMostCalories, getTopThreeCaloriesTotal}

import scala.collection.mutable
import scala.collection.mutable.Stack

object day5 {
  case class Instruction(amount: Int, from: Int, to: Int)

  def getCrateStacks(inputArr: Array[String]): Array[mutable.Stack[String]] = {
    val crateStacks = inputArr.last.strip.split("\\s+").map(_ => new mutable.Stack[String]())
    inputArr.init.reverse.foreach(row => {
      row.sliding(3, 4).zipWithIndex
        .foreach{ case (crate, idx) =>
          if (!crate.isBlank) {
            crateStacks(idx).push("[A-Z]".r.findFirstIn(crate).get)
        }
      }
    })
    crateStacks
  }

  def getInstructions(inputArr: Array[String]): Array[Instruction] = {
    inputArr.map(row => "\\d+".r.findAllIn(row).toList
      .map(digits => digits.toInt)
    ).map(numbers => Instruction(numbers.head, numbers(1) - 1, numbers(2) - 1))
  }

  def executeInstruction(crateStacks: Array[mutable.Stack[String]], instruction: Instruction): Unit = {
    0.until(instruction.amount).foreach(_ => {
      crateStacks(instruction.to).push(crateStacks(instruction.from).pop())
    })
  }

  def findTops(input: String): String = {
    val crateStacksAndInstructions = input.split("\n\n").map(section => section.split("\n"))
    val crateStacks = getCrateStacks(crateStacksAndInstructions(0))
    val instructions = getInstructions(crateStacksAndInstructions(1))
    instructions.foreach(instruction => {
      executeInstruction(crateStacks, instruction)
    })
    crateStacks.map(stack => stack.top).mkString
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day5.txt")
    val input = try source.mkString finally source.close()
    println(s"Part 1 answer: ${findTops(input)}")
  }
}
