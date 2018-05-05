package main.scala.uit.ai.model

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

class MinimaxTree[Node] {

  val WIN_VALUE = 100000
  val FOUR_VALUE = 50000
  val THREE_VALUE = 10000
  val TWO_VALUE = 100
  val ONE_VALUE = 1

  class Node(
    val level: Int,
    val player: Boolean,
    val isMax: Boolean,
    val isLeaf: Boolean,
    val move: (Int, Int),
    var value: Int,
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

  }

  object Node {
    def getCandidates(board: Array[Array[Option[Boolean]]]): List[(Int, Int)] = {
      val rowCount = board.length
      val columnCount = if (board.isEmpty) 0 else board(0).length

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

    def getStateAfterMove(board: Array[Array[Option[Boolean]]], r: Int, c: Int, player: Boolean) = {
      val rowCount = board.length
      val columnCount = if (board.isEmpty) 0 else board(0).length
      val cloneBoard: Array[Array[Option[Boolean]]] = Array.fill(rowCount, columnCount)(null)
      for (i <- 0 until rowCount)
        for (j <- 0 until columnCount)
          cloneBoard(i)(j) = board(i)(j)
      cloneBoard(r)(c) = Option(player)
      cloneBoard
    }

    /*def generateChildren(board: Array[Array[Option[Boolean]]],player:Boolean,maxLevel: Int, hasBlock: Boolean) = {
      for (e <- getCandidates(board)) {
        val state = getStateAfterMove(board,e._1, e._2, !player)
        var node: Node = null
        var value = 0
        val checkLeaf = {
          Node.checkWinAtState(numInARowNeeded, state, e, rowCount, columnCount, hasBlock) match {
            case Some(true) => {
              value = WIN_VALUE // I win
              true
            }
            case Some(false) => {
              value = -WIN_VALUE // Opponent win
              true
            }
            case null => false
            case None => false
          }
        }
        if (value == 0) {
          if (isMax)
            node = new Node(level + 1, !player, !isMax, checkLeaf, e, Int.MaxValue, state, List.empty[Node])
          else
            node = new Node(level + 1, !player, !isMax, checkLeaf, e, Int.MinValue, state, List.empty[Node])
        } else {
          node = new Node(level + 1, !player, !isMax, checkLeaf, e, value, state, List.empty[Node])
        }
        children = children.+:(node)
      }
    }*/

    def calculateValue(node: Node, hasBlock: Boolean): Int = {
      if (node.isLeaf) return node.value
      else {
        var point = 0
        val bufferMove = new ArrayBuffer[Array[Option[Boolean]]]()
        bufferMove.append(Node.getRow(node.board, node.move))
        bufferMove.append(Node.getColumn(node.board, node.move, node.rowCount))
        bufferMove.append(Node.getLTR(node.board, node.move, node.rowCount, node.columnCount))
        bufferMove.append(Node.getRTL(node.board, node.move, node.rowCount, node.columnCount))

        val arrayMove = bufferMove.toArray

        arrayMove.foreach(row => {
          val side = Node.nInARow(1, row, hasBlock)
          if (side == Option(true)) // Me
            point += ONE_VALUE
          else if (side == Option(false)) // My Opponent
            point -= ONE_VALUE
        })

        arrayMove.foreach(row => {
          val side = Node.nInARow(2, row, hasBlock)
          if (side == Option(true)) // Me
            point += TWO_VALUE
          else if (side == Option(false)) // My Opponent
            point -= TWO_VALUE
        })

        arrayMove.foreach(row => {
          val side = Node.nInARow(3, row, hasBlock)
          if (side == Option(true)) // Me
            point += THREE_VALUE
          else if (side == Option(false)) // My Opponent
            point -= THREE_VALUE
        })

        arrayMove.foreach(row => {
          val side = Node.nInARow(4, row, hasBlock)
          if (side == Option(true)) // Me
            point += FOUR_VALUE
          else if (side == Option(false)) // My Opponent
            point -= FOUR_VALUE
        })

        //value = point

        return point
      }
    }

    def getRow(board: Array[Array[Option[Boolean]]], move: (Int, Int)): Array[Option[Boolean]] = {
      board(move._1)
    }

    def getColumn(board: Array[Array[Option[Boolean]]], move: (Int, Int), rowCount: Int): Array[Option[Boolean]] = {
      //val rowCount = board.length
      (for (r <- 0 until rowCount) yield board(r)(move._2)).toArray
    }

    def getLTR(board: Array[Array[Option[Boolean]]], move: (Int, Int), rowCount: Int, columnCount: Int): Array[Option[Boolean]] = {
      //val rowCount = board.length
      //val columnCount = if (board.isEmpty) 0 else board(0).length
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

    def getRTL(board: Array[Array[Option[Boolean]]], move: (Int, Int), rowCount: Int, columnCount: Int): Array[Option[Boolean]] = {
      val rowCount = board.length
      val columnCount = if (board.isEmpty) 0 else board(0).length
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

    def checkWinAtState(numInARowNeeded: Int, state: Array[Array[Option[Boolean]]], move: (Int, Int), rowCount: Int, columnCount: Int, hasBlock: Boolean): Option[Boolean] = {
      val row = nInARow(numInARowNeeded, getRow(state, move), hasBlock)
      val column = nInARow(numInARowNeeded, getColumn(state, move, rowCount), hasBlock)
      val ltr = nInARow(numInARowNeeded, getLTR(state, move, rowCount, columnCount), hasBlock)
      val rtl = nInARow(numInARowNeeded, getRTL(state, move, rowCount, columnCount), hasBlock)

      if (row != null) return row
      else if (column != null) return column
      else if (ltr != null) return ltr
      else if (rtl != null) return rtl
      else return null
    }
  }

  private var root: Node = null

  def setRootNode(board: Array[Array[Option[Boolean]]]) {
    root = new Node(0, false, isMax = false, false, null, Int.MaxValue, board.clone(), List.empty[Node])
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

  private def minimize(node: Node, depth: Int, alpha: Int, beta: Int, hasBlock: Boolean): Int = {
    //println("Min: " + depth)
    if (node.isLeaf || depth == 0) return Node.calculateValue(node, hasBlock) // calculate node value
    var newBeta = beta
    node.children.foreach(child => {
      //println("min")
      newBeta = math.min(newBeta, maximize(child, depth - 1, alpha, newBeta, hasBlock))
      if (alpha >= newBeta) {
        node.value = newBeta
        return alpha
      }
    })
    node.value = newBeta
    newBeta
  }

  private def maximize(node: Node, depth: Int, alpha: Int, beta: Int, hasBlock: Boolean): Int = {
    //println("Max: " + depth)
    if (node.isLeaf || depth == 0) return Node.calculateValue(node, hasBlock) // calculate node value
    var newAlpha = alpha
    node.children.foreach(child => {
      //println("max")
      newAlpha = math.max(newAlpha, minimize(child, depth - 1, newAlpha, beta, hasBlock))
      if (newAlpha >= beta) {
        node.value = newAlpha
        return beta
      }
    })
    node.value = newAlpha
    newAlpha
  }

  def evaluateTreeWithAlphaBeta(numberOfLevel: Int, hasBlock: Boolean) = {
    minimize(root, numberOfLevel, Integer.MIN_VALUE, Integer.MAX_VALUE, hasBlock)
  }

  def getBestMove(): (Int, Int) = {
    var maxValue = Int.MinValue
    var maxNode: Node = null
    root.children.foreach(child => {
      println("Child value: " + child.value)
      if (child.value > maxValue) {
        maxNode = child
        maxValue = child.value
      }
    })
    maxNode.move
  }
}

