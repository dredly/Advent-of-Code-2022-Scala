object day2 {

  case class RPSHand(shape: String, representedBy: Array[String], beats: String, points: Int)

  val rock: RPSHand = RPSHand("ROCK", Array("A", "X"), "SCISSORS", 1)
  val paper: RPSHand = RPSHand("PAPER", Array("B", "Y"), "ROCK", 2)
  val scissors: RPSHand = RPSHand("SCISSORS", Array("C", "Z"), "PAPER", 3)

  val possibleHands: Array[RPSHand] = Array(rock, paper, scissors)

  val resultsMap: Map[String, Int] = Map(
    "X" -> 0,
    "Y" -> 3,
    "Z" -> 6
  )

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

  def getScoreByResult(opponentSymbol: String, resultSymbol: String): Int = {
    val opponentHand = possibleHands.find(hand => hand.representedBy.contains(opponentSymbol)).get
    resultSymbol match
      case "X" => possibleHands.find(hand => opponentHand.beats == hand.shape).get.points // We need to lose
      case "Y" => possibleHands.find(hand => opponentHand.shape == hand.shape).get.points // We need to draw
      case "Z" => possibleHands.find(hand => hand.beats == opponentHand.shape).get.points // We need to win
  }

  def getScore(opponentHand: RPSHand, myHand: RPSHand): Int = {
    myHand.points + getScoreByResult(opponentHand, myHand)
  }

  def getScore(roundRepr: String): Int = {
    roundRepr.length match
      case 2 =>
        val opponentSymbol = roundRepr.substring(0, 1)
        val resultSymbol = roundRepr.substring(1)
        resultsMap(resultSymbol) + getScoreByResult(opponentSymbol, resultSymbol)
      case _ => throw new Exception("Wrong length")
  }

  def getTotalScore(inputList: List[String]): Int = {
    inputList
      .map(entry => entry.replace(" ", ""))
      .map(entry => getHands(entry))
      .map(hands => getScore(hands(0), hands(1)))
      .sum
  }

  def getTotalScorePart2(inputList: List[String]): Int = {
    inputList
      .map(entry => entry.replace(" ", ""))
      .map(entry => getScore(entry))
      .sum
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day2.txt")
    val input = try source.getLines().toList finally source.close()
    println(s"Part 1 answer: ${getTotalScore(input)}")
    println(s"Part 2 answer: ${getTotalScorePart2(input)}")
  }
}
