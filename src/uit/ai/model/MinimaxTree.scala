package uit.ai.model

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

class MinimaxTree[Node] {
  class Node(
    val level: Int,
    val player: Boolean,
    val isMax: Boolean,
    val isLeaf: Boolean,
    val move: (Int, Int),
    var value: Option[Int],
    val board: Array[Array[Option[Boolean]]],
    var children: Seq[Node]) {

    val rowCount = board.length
    val columnCount = if (board.isEmpty) 0 else board(0).length

    val numInARowNeeded: Int = {
      // so nuoc di thang hang can de chien thang
      // numbers chosen rather arbitrarily by me. I looked at this: http://en.wikipedia.org/wiki/M,n,k-game
      // and tried to pick numbers that more or less made sense
      if (rowCount <= 3 || columnCount <= 3) {
        // tic tac toe or bizarre tiny variants
        scala.math.min(rowCount, columnCount)
      } else if (rowCount <= 5) {
        // connect 4, sort of
        4
      } else {
        // gomoku
        5
      }
    }

    def getRow(move: (Int, Int)): Array[Option[Boolean]] = {
      board(move._1)
    }

    def getColumn(move: (Int, Int)): Array[Option[Boolean]] = {
      (for (r <- 0 until rowCount) yield board(r)(move._2)).toArray
    }

    def getLTR(move: (Int, Int)): Array[Option[Boolean]] = {
      val resulfBuffer = new ArrayBuffer[Option[Boolean]]
      // start point
      var start = (0, 0)
      if (move._1 > move._2) start = (move._1 - move._2, 0) // neu x > y
      else start = (0, move._2 - move._1)
      var i = 0
      if (rowCount - 1 - move._1 > columnCount - 1 - move._2) { // diem cuoi cham canh doc lon nhat (column max)
        for (col <- start._2 until columnCount) {
          resulfBuffer.append(board(start._1 + i)(col))
          i += 1
        }
      } else { // diem cuoi cham canh ngang lon nhat (row max)
        for (row <- start._1 until rowCount) {
          resulfBuffer.append(board(row)(start._2 + i))
          i += 1
        }
      }

      resulfBuffer.toArray
    }

    def getRTL(move: (Int, Int)): Array[Option[Boolean]] = {
      val resulfBuffer = new ArrayBuffer[Option[Boolean]]
      // start point
      var start = (0, 0)
      if (move._2 > rowCount - 1 - move._1)
        start = (rowCount - 1, move._1 + move._2 - rowCount + 1)
      else
        start = (move._1 + move._2, 0)
      var i = 0
      if (move._1 < columnCount - 1 - move._2) { // diem cuoi cham hang be nhat (row 0)
        for (row <- start._1 to 0 by -1) {
          resulfBuffer.append(board(row)(start._2 + i))
          i += 1
        }
      } else { // diem cuoi cham canh doc lon nhat (column max)
        for (col <- start._2 until columnCount) {
          resulfBuffer.append(board(start._1 - i)(col))
          i += 1
        }
      }

      resulfBuffer.toArray
    }

    def nInARow(n: Int, array: Array[Option[Boolean]], hasBlock: Boolean): Option[Boolean] = {
      for (i <- 0 until array.length - (n - 1)) {
        var allTrue = true;
        for (j <- i + 1 until i + n) {
          allTrue &= array(j - 1) == array(j)
        }
        if (allTrue && array(i) != null) {
          if (hasBlock) {
            if (i > 0 && i + n < array.length && array(i - 1) != array(i) && array(i + n) != array(i) && array(i - 1) != null && array(i + n) != null) // dieu kien chan hai dau thi khong thang
              return null
            else
              return array(i)
          } else
            return array(i)
        }
      }

      return null
    }

    def checkWinAtState(hasBlock: Boolean): Int = {
      var value = 0 // tie
      val row = getRow(move)
      val column = getColumn(move)
      val ltr = getLTR(move)
      val rtl = getRTL(move)
      return nInARow(numInARowNeeded, row, hasBlock)
    }

    def getBoard = board

    def getCandidates(): List[(Int, Int)] = {
      val candidates = new ListBuffer[(Int, Int)] //set of candidates
      val nonAvailableElems = new ListBuffer[(Int, Int)] //set of non available movements
      // get all non-available moves
      for (r <- 0 until rowCount)
        for (c <- 0 until columnCount)
          if (board(r)(c) != null)
            nonAvailableElems.append((r, c))
      // check around to get candidates
      for (e <- nonAvailableElems) {
        if (e._1 + 1 < rowCount && !nonAvailableElems.contains((e._1 + 1, e._2)) && !candidates.contains((e._1 + 1, e._2))) //East
          candidates.append((e._1 + 1, e._2))
        if (e._1 - 1 >= 0 && !nonAvailableElems.contains((e._1 - 1, e._2)) && !candidates.contains((e._1 - 1, e._2))) //West
          candidates.append((e._1 - 1, e._2))
        if (e._2 - 1 >= 0 && !nonAvailableElems.contains((e._1, e._2 - 1)) && !candidates.contains((e._1, e._2 - 1))) //North
          candidates.append((e._1, e._2 - 1))
        if (e._2 + 1 < columnCount && !nonAvailableElems.contains((e._1, e._2 + 1)) && !candidates.contains((e._1, e._2 + 1))) //South
          candidates.append((e._1, e._2 + 1))
        if (e._1 + 1 < rowCount && e._2 - 1 >= 0 && !nonAvailableElems.contains((e._1 + 1, e._2 - 1)) && !candidates.contains((e._1 + 1, e._2 - 1))) //East-North
          candidates.append((e._1 + 1, e._2 - 1))
        if (e._1 + 1 < rowCount && e._2 + 1 < columnCount && !nonAvailableElems.contains((e._1 + 1, e._2 + 1)) && !candidates.contains((e._1 + 1, e._2 + 1))) //East-South
          candidates.append((e._1 + 1, e._2 + 1))
        if (e._1 - 1 >= 0 && e._2 + 1 < columnCount && !nonAvailableElems.contains((e._1 - 1, e._2 + 1)) && !candidates.contains((e._1 - 1, e._2 + 1))) //West-South
          candidates.append((e._1 - 1, e._2 + 1))
        if (e._1 - 1 >= 0 && e._2 - 1 >= 0 && !nonAvailableElems.contains((e._1 - 1, e._2 - 1)) && !candidates.contains((e._1 - 1, e._2 - 1))) //West-North
          candidates.append((e._1 - 1, e._2 - 1))
      }
      //println("Candidate size: " + candidates.size)
      candidates.toList
    }

    def updateAndGet(r: Int, c: Int, player: Boolean) = {
      val cloneBoard: Array[Array[Option[Boolean]]] = Array.fill(rowCount, columnCount)(null)
      for (i <- 0 until rowCount)
        for (j <- 0 until columnCount)
          cloneBoard(i)(j) = board(i)(j)
      cloneBoard(r)(c) = Option(player)
      cloneBoard
    }

    def generateChildren(maxLevel: Int) = {
      for (e <- getCandidates()) {
        val state = updateAndGet(e._1, e._2, player)
        /*val checkLeaf = {
          if (determineWinner(state) != null) true
          else if (level + 1 == maxLevel) true else false
        }*/
        var node: Node = new Node(level + 1, player, !isMax, false, e, null, state, List.empty[Node])
        children = children.+:(node)
      }
      //children
    }

    def evaluateState() = {
      -10000
    }
  }

  private var root: Node = null

  def setRootNode(board: Array[Array[Option[Boolean]]]) {
    root = new Node(0, false, isMax = false, false, null, null, board.clone(), List.empty[Node])
  }

  def preorder(visit: Node => Unit) {
    def recur(n: Node) {
      visit(n)
      for (c <- n.children) recur(c)
    }
    recur(root)
  }

  def postorder(visit: Node => Unit) {
    def recur(n: Node) {
      for (c <- n.children) recur(c)
      visit(n)
    }
    recur(root)
  }

  def height(n: Node): Int = {
    1 + n.children.foldLeft(-1)((h, c) => h max height(c))
  }

  def size(n: Node): Int = {
    1 + n.children.foldLeft(0)((s, c) => s + size(c))
  }

  def fillInTheTree(numberOfLevel: Int): Unit = {
    preorder { n =>
      {
        if (n.level < numberOfLevel && !n.isLeaf) {
          println("Level: " + n.level)
          n.generateChildren(numberOfLevel)
        }
      }
    }
  }
}