import scala.annotation.tailrec

object day11 {
  case class Monkey(
                     items: Array[Int],
                     operation: Function[Int, Int],
                     divTest: Function[Int, Boolean],
                     throwToIfTrue: Int,
                     throwToIfFalse: Int,
                     numInspected: Int
                   )

  def parseOperation(inputStr: String): Function[Int, Int] = {
    inputStr.substring(4, 5) match
      case "+" => x => x + inputStr.substring(6).toIntOption.getOrElse(x)
      case "*" => x => x * inputStr.substring(6).toIntOption.getOrElse(x)
      case "_" => throw Exception("Could not parse operation")
  }

  def makeDivisibiltyTest(divisibleBy: Int): Function[Int, Boolean] = {
    x => x % divisibleBy == 0
  }

  def parseMonkey(text: String): Monkey = {
    val lines = text.split("\n").map(line => line.stripIndent())

    val startingItems = lines(1).split(": ")(1).split(", ").map(digits => digits.toInt)
    val operation = parseOperation(lines(2).split(" = ")(1))
    val divTest = makeDivisibiltyTest(lines(3).split(" by ")(1).toInt)
    val throwToIfTrue = lines(4).split("monkey ")(1).toInt
    val throwToIfFalse = lines(5).split("monkey ")(1).toInt

    // Initialise monkeys with a value of 0 for numInspected
    Monkey(startingItems, operation, divTest, throwToIfTrue, throwToIfFalse, 0)
  }

  def parseMonkeys(input: String): Array[Monkey] = {
    input.split("\n\n").map(text => parseMonkey(text))
  }

  def throwItem(from: Monkey, to: Array[Int]): (Array[Int], Array[Int]) = {
    (from.items.drop(1), to :+ from.operation(from.items.head) / 3)
  }

  @tailrec
  def takeTurn(monkeys: Array[Monkey], idx: Int): Array[Monkey] = {
    monkeys(idx).items.length match
      case 0 => monkeys
      case _ =>
        val monkey = monkeys(idx)
        val worryLevel = monkey.operation(monkey.items.head) / 3
        val throwToIdx = if monkey.divTest(worryLevel) then monkey.throwToIfTrue else monkey.throwToIfFalse
        val throwTo = monkeys(throwToIdx)
        val throwResults = throwItem(monkey, throwTo.items)
        val updatedMonkeys = monkeys.zipWithIndex.map{ case(m, i) =>
          if i == idx then m.copy(items = throwResults(0), numInspected = m.numInspected + 1) else {
            if i == throwToIdx then m.copy(items = throwResults(1)) else m
          }
        }
        takeTurn(updatedMonkeys, idx)
  }

  @tailrec
  def doRound(monkeys: Array[Monkey], idx: Int = 0): Array[Monkey] = {
    idx - monkeys.length match
      case 0 => monkeys
      case _ => doRound(takeTurn(monkeys, idx), idx + 1)
  }

  @tailrec
  def doNRounds(monkeys: Array[Monkey], roundsLeft: Int): Array[Monkey] = {
    roundsLeft match
      case 0 => monkeys
      case _ => doNRounds(doRound(monkeys), roundsLeft - 1)
  }

  def getMonkeyBusiness(inputStr: String, numRounds: Int): Int = {
    doNRounds(parseMonkeys(inputStr), numRounds)
      .map(m => m.numInspected)
      .sorted
      .takeRight(2)
      .product
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day11.txt")
    val input = try source.mkString finally source.close()
    println(s"Part 1 answer = ${getMonkeyBusiness(input, 20)}")
  }
}
