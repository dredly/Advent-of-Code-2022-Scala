import scala.annotation.tailrec

object day8 {
  case class PineTree(height: Int, idx: Int)

  def makeTreeMatrix(inputStr: String): Array[Array[PineTree]] = {
    val rows = inputStr.split("\n")
    val width = rows(0).length
    inputStr
      .replaceAll("\n", "")
      .toCharArray
      .zipWithIndex.map((chr, idx) => PineTree(chr.toString().toInt, idx))
      .grouped(width)
      .toArray
  }

  @tailrec
  def scanRow(startHeight: Int, row: Array[PineTree], visible: Array[PineTree] = Array()): Set[PineTree] = {
    row.length match
      case 0 => visible.toSet
      case _ =>
        if row.head.height > startHeight
        then scanRow(row.head.height, row.tail, visible :+ row.head)
        else scanRow(startHeight, row.tail, visible)
  }

  def getVisibileInnerTreesForRow(row: Array[PineTree]): Set[PineTree] = {
    val fromLeft = scanRow(row.head.height, row.tail.init)
    val fromRight = scanRow(row.last.height, row.tail.init.reverse)
    fromLeft.union(fromRight)
  }

  def getNumOfOuterTrees(width: Int, height: Int): Int = {
    2 * width + 2 * (height - 2)
  }

  def visibleInOrientation(forest: Array[Array[PineTree]]): Set[PineTree] = {
    forest.init.tail.map(getVisibileInnerTreesForRow).reduce((s1, s2) => s1.union(s2))
  }

  def getNumOfVisibleTrees(inputStr: String): Int = {
    val forest = makeTreeMatrix(inputStr)
    val horizontallyVisible = visibleInOrientation(forest)
    val verticallyVisible = visibleInOrientation(forest.transpose)
    horizontallyVisible.union(verticallyVisible).size
      .+(getNumOfOuterTrees(forest(0).length, forest.length))
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day8.txt")
    val input = try source.mkString finally source.close()
    println(s"Part 1 answer: ${getNumOfVisibleTrees(input)}")
  }
}
