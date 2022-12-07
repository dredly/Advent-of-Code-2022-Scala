import scala.annotation.tailrec
import scala.collection.immutable.HashMap

object day7 {
  sealed trait Node

  case class DirectoryNode(name: String, parent: Option[DirectoryNode]) extends Node

  case class FileNode(name: String, parent: DirectoryNode, size: Int) extends Node

  def parseDir(line: String, parent: Option[DirectoryNode]): DirectoryNode = {
    DirectoryNode(line.substring(4), parent)
  }

  def parseFile(line: String, parent: DirectoryNode): FileNode = {
    val splitLine = line.split(" ")
    FileNode(splitLine(1), parent, splitLine(0).toInt)
  }

  @tailrec
  def getFileList(
                   linesRemaining: Array[String],
                   dirList: List[DirectoryNode] = List(DirectoryNode("/", Option.empty)),
                   fileList: List[FileNode] = List(),
                   currentDirectory: DirectoryNode = DirectoryNode("/", Option.empty)
                 ): List[FileNode] = {
    linesRemaining.length match
      case 0 => fileList
      case _ =>
        val currentLine = linesRemaining.head
        currentLine.substring(0, 4) match
          case "dir " =>
            val newDir = parseDir(currentLine, Option(currentDirectory))
            val updatedDirList = if dirList.contains(newDir) then dirList else dirList :+ newDir
            getFileList(linesRemaining.tail, updatedDirList, fileList, currentDirectory)
          case "$ cd" =>
            val cdTo: DirectoryNode = if currentLine.substring(5) == ".."
              then currentDirectory.parent.get
              else DirectoryNode(currentLine.substring(5), Option(currentDirectory))
            getFileList(linesRemaining.tail, dirList, fileList, cdTo)
          case "$ ls" => getFileList(linesRemaining.tail, dirList, fileList, currentDirectory) //Done
          case _ =>
            val file = parseFile(currentLine, currentDirectory)
            val updatedFileList = if fileList.contains(file) then fileList else fileList :+ file
            getFileList(linesRemaining.tail, dirList, updatedFileList, currentDirectory)
  }

  @tailrec
  def getParentSizeContributions(node: Node, mapSoFar: HashMap[DirectoryNode, Int] = HashMap[DirectoryNode, Int]()): HashMap[DirectoryNode, Int] = {
    node match
      case dn: DirectoryNode =>
        dn.parent match
          case None => mapSoFar
          case Some(parent) => getParentSizeContributions(parent, mapSoFar.+(parent -> mapSoFar(dn)))
      case fn: FileNode => getParentSizeContributions(fn.parent, mapSoFar.+(fn.parent -> fn.size))
  }

  def getSizes(inputArr: Array[String]): Iterable[Int] = {
    getFileList(inputArr.tail)
      .map(fn => getParentSizeContributions(fn))
      .reduce((m1, m2) => m1.merged(m2)({ case ((k, v1), (_, v2)) => (k, v1 + v2) }))
      .values
  }

  def getTotalSize(inputArr: Array[String]): Int = {
    getSizes(inputArr).filter(size => size <= 100000).sum
  }

  def getSmallestToDelete(inputArr: Array[String], totalDiskSpace: Int, spaceNeeded: Int): Int = {
    getSizes(inputArr)
      .filter(size => size >= getSizes(inputArr).max - (totalDiskSpace - spaceNeeded))
      .min
  }

  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("day7.txt")
    val input = try source.getLines().toArray finally source.close()
    println(s"Part 1 answer: ${getTotalSize(input)}")
    println(s"Part 2 answer: ${getSmallestToDelete(input, 70000000, 30000000)}")
  }

}
