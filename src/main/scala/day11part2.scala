import scala.collection.mutable

object day11part2 {
  case class Monkey(
                     items: mutable.Queue[Long],
                     operation: Function[Long, Long],
                     divTestNumber: Int,
                     divTest: Function[Long, Boolean],
                     throwToIfTrue: Int,
                     throwToIfFalse: Int,
                     var numInspected: Long
                   )

  def parseOperation(inputStr: String): Function[Long, Long] = {
    inputStr.substring(4, 5) match
      case "+" => x
        => x + inputStr.substring(6).toLongOption.getOrElse(x)
      case "*" => x
        => x * inputStr.substring(6).toLongOption.getOrElse(x)
      case "_" => throw Exception("Could not parse operation")
  }

  def makeDivisibiltyTest(divisibleBy: Int): Function[Long, Boolean] = {
    x => x % divisibleBy == 0
  }

  def parseMonkey(text: String): Monkey = {
    val lines = text.split("\n").map(line => line.stripIndent())

    val startingItems = lines(1).split(": ")(1).split(", ").map(digits => digits.toLong)
    val operation = parseOperation(lines(2).split(" = ")(1))
    val divTestNumber = lines(3).split(" by ")(1).toInt
    val divTest = makeDivisibiltyTest(divTestNumber)
    val throwToIfTrue = lines(4).split("monkey ")(1).toInt
    val throwToIfFalse = lines(5).split("monkey ")(1).toInt

    Monkey(mutable.Queue(startingItems:_*), operation, divTestNumber, divTest, throwToIfTrue, throwToIfFalse, 0)
  }

  def parseMonkeys(input: String): Array[Monkey] = {
    input.split("\n\n").map(text => parseMonkey(text))
  }

  def throwItem(from: Monkey, monkeys: Array[Monkey], leastCommonMultiple: Int): Unit = {
    val thrown = from.items.dequeue()
    val received = from.operation(thrown) % leastCommonMultiple
    val throwToIdx = if from.divTest(received) then from.throwToIfTrue else from.throwToIfFalse
    val throwTo = monkeys(throwToIdx)
    throwTo.items.enqueue(received)
  }

  def takeTurn(monkeys: Array[Monkey], idx: Int, leastCommonMultiple: Int): Unit = {
    val monkey = monkeys(idx)
    monkey.numInspected = monkey.numInspected + monkey.items.length
    while (monkey.items.nonEmpty) {
      throwItem(monkey, monkeys, leastCommonMultiple)
    }
  }

  def doRound(monkeys: Array[Monkey], leastCommonMultiple: Int): Unit = {
     monkeys.indices.foreach(idx => takeTurn(monkeys, idx, leastCommonMultiple))
  }

  def doNRounds(monkeys: Array[Monkey], numRounds: Int): Unit = {
    val leastCommonMultiple = monkeys.map(m => m.divTestNumber).product
    for (_ <- 0.until(numRounds)) {
      doRound(monkeys, leastCommonMultiple)
    }
  }

  def getMonkeyBusiness(inputStr: String, numRounds: Int): Long = {
    val monkeys = parseMonkeys(inputStr)
    doNRounds(monkeys, numRounds)

    monkeys
      .map(m => m.numInspected)
      .sorted
      .takeRight(2)
      .product
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day11.txt")
    val input = try source.mkString finally source.close()
    println(s"Part 2 answer = ${getMonkeyBusiness(input, 10000)}")
  }
}
