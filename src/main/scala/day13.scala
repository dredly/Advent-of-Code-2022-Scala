import scala.annotation.tailrec

object day13 {

  def turnFirstElementIntoList(packet: String): String = {
    if packet.length > 1 & packet.substring(0, 2).toIntOption.isDefined then {
      "[" ++ packet.substring(0, 2) ++ "]" ++ packet.drop(2)
    } else "[" ++ packet.substring(0, 1) ++ "]" ++ packet.drop(1)

  }

  @tailrec
  def comparePackets(p1: String, p2: String): Boolean = {
    val sameChar = raw"(.)\1".r
    val sqBracketThenNum = raw"\[\d".r
    val numThenSqBracket = raw"\d\[".r
    val bothNums = raw"\d{2}".r
    val numThenNotNum = raw"\d\D".r
    val notNumThenNum = raw"\D\d".r
    val closingThenSomethingElse = raw"].".r
    val somethingElseThenClosing = ".]".r

    if p1.isBlank then true else { if p2.isBlank then false else {
      p1.substring(0, 1) ++ p2.substring(0, 1) match
        case sameChar(_*) => comparePackets(p1.tail, p2.tail)
        case sqBracketThenNum(_*) => comparePackets(p1, turnFirstElementIntoList(p2))
        case numThenSqBracket(_*) => comparePackets(turnFirstElementIntoList(p1), p2)
        case bothNums(_*) =>
          // Handle the possibility of 2 digit numbers
          val p1FullNumber =
            if p1.length > 1 & p1.substring(0, 2).toIntOption.isDefined
            then p1.substring(0, 2).toInt else p1.substring(0, 1).toInt
          val p2FullNumber =
            if p2.length > 1 & p2.substring(0, 2).toIntOption.isDefined
            then p2.substring(0, 2).toInt else p2.substring(0, 1).toInt

          p2FullNumber > p1FullNumber
        case numThenNotNum(_*) => false
        case notNumThenNum(_*) => true
        case closingThenSomethingElse(_*) => true
        case somethingElseThenClosing(_*) => false
        case _ => throw Exception("Did not match any of the expected cases")
    }}
  }

  def getPacketPairs(inputStr: String): Array[(String, String)] = {
    inputStr.split("\n\n").map(lines => (lines.split("\n")(0), lines.split("\n")(1)))
  }

  def getSumOfCorrectIndices(inputStr: String): Int = {
    getPacketPairs(inputStr)
      .map(pp => comparePackets(pp._1, pp._2))
      .zipWithIndex
      .filter { case (result, _) => result }
      .map { case (_, idx) => idx + 1 }
      .sum
  }

  def getDecoderKey(inputStr: String, dividerPackets: (String, String)): Int = {
    val sortedPackets = inputStr.replaceAll("\n\n", "\n").split("\n")
      .++(Array(dividerPackets._1, dividerPackets._2))
      .sortWith((p1, p2) => comparePackets(p1, p2))
    (sortedPackets.indexWhere(p => p == dividerPackets._1) + 1) * (sortedPackets.indexWhere(p => p == dividerPackets._2) + 1)
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day13.txt")
    val input = try source.mkString finally source.close()
    println(s"Part 1 answer: ${getSumOfCorrectIndices(input)}")
    val startTime = System.currentTimeMillis()
    println(s"Part 2 answer: ${getDecoderKey(input, ("[[2]]", "[[6]]"))}")
    println(s"Part 2 took ${System.currentTimeMillis() - startTime} ms")
  }
}
