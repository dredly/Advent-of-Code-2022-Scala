import scala.annotation.tailrec

object day11 {
  case class Monkey(
                     items: Array[Int],
                     operation: Function[Int, Int],
                     divTest: Function[Int, Boolean],
                     throwToIfTrue: Int,
                     throwToIfFalse: Int
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

    Monkey(startingItems, operation, divTest, throwToIfTrue, throwToIfFalse)
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
        val worryLevel = monkey.operation(monkey.items.head)
        println(s"Monkey number $idx, worry level of $worryLevel")
        val throwToIdx = if monkey.divTest(worryLevel) then monkey.throwToIfTrue else monkey.throwToIfFalse
        val throwTo = monkeys(throwToIdx)
        val throwResults = throwItem(monkey, throwTo.items)
        val updatedMonkeys = monkeys.zipWithIndex.map{ case(m, i) =>
          if i == idx then m.copy(items = throwResults(0)) else {
            if i == throwToIdx then m.copy(items = throwResults(1)) else m
          }
        }
        val myNewWorryLevel = monkey.operation(monkey.items.head) / 3
        println(s"My new worry level = $myNewWorryLevel")
        takeTurn(updatedMonkeys, idx)
  }

  @tailrec
  def doRound(monkeys: Array[Monkey], idx: Int = 0): Array[Monkey] = {
    println(s"Turn $idx")
    idx - monkeys.length match
      case 0 => monkeys
      case _ => doRound(takeTurn(monkeys, idx), idx + 1)
  }

//  def doRoundBad(monkeys: Array[Monkey]): Array[Monkey] = {
//    val after0 = takeTurn(monkeys, 0)
//    val after1 = takeTurn(after0, 1)
//    val after2 = takeTurn(after1, 2)
//    val after3= takeTurn(after2, 3)
//    after3
//  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day11.txt")
    val input = try source.mkString finally source.close()
    val monkeys = parseMonkeys(input)
    //val updatedMonkeys = takeTurn(monkeys, 0)
    //val updatedMonkeys = doRoundBad(monkeys)
    val updatedMonkeys = doRound(monkeys)
    println(updatedMonkeys.mkString("Array(", ", ", ")"))
  }
}
