object day2 {

  case class RPSHand(shape: String, representedBy: Array[String], beats: String, points: Int)

  val rock: RPSHand = RPSHand("ROCK", Array("A", "X"), "SCISSORS", 1)
  val paper: RPSHand = RPSHand("PAPER", Array("B", "Y"), "ROCK", 2)
  val scissors: RPSHand = RPSHand("SCISSORS", Array("C", "Z"), "PAPER", 3)

  val possibleHands: Array[RPSHand] = Array(rock, paper, scissors)

  def getHands(roundRepr: String): Array[RPSHand] = {
    roundRepr.length match
      case 2 =>
        roundRepr.split("").map(symbol => possibleHands.find(hand => hand.representedBy.contains(symbol)).get)
      case _ => throw new Exception("Wrong length")
  }

  def getScoreByResult(opponentHand: RPSHand, myHand: RPSHand): Int = {
    myHand match
      case myHand if myHand.shape == opponentHand.shape => 3
      case myHand if myHand.beats == opponentHand.shape => 6
      case _ => 0
  }

  def getScore(opponentHand: RPSHand, myHand: RPSHand): Int = {
    myHand.points + getScoreByResult(opponentHand, myHand)
  }

  def getTotalScore(inputList: List[String]): Int = {
    inputList
      .map(entry => entry.replace(" ", ""))
      .map(entry => getHands(entry))
      .map(hands => getScore(hands(0), hands(1)))
      .sum
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day2.txt")
    val input = try source.getLines().toList finally source.close()
    println(s"Part 1 answer: ${getTotalScore(input)}")
  }
}
