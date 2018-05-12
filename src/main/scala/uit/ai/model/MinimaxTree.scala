package main.scala.uit.ai.model

import scala.collection.mutable.ArrayBuffer

class MinimaxTree[Node] {

  class Node(
    val level: Int,
    val player: Boolean,
    val move: (Int, Int),
    val board: Array[Array[Option[Boolean]]]) {

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

    val WIN_VALUE = 100000
    val DIVIDE_RATIO = 7

    /*var win = [
        [1, 1, 1, 1, 1]
      ];
      var unCovered4 = [
        [0, 1, 1, 1, 1, 0]
      ];
      var unCovered3 = [
        [0, 1, 1, 1, 0, 0],
        [0, 0, 1, 1, 1, 0],
        [0, 1, 0, 1, 1, 0],
        [0, 1, 1, 0, 1, 0]
      ];
      var unCovered2 = [
        [0, 0, 1, 1, 0, 0],
        [0, 1, 0, 1, 0, 0],
        [0, 0, 1, 0, 1, 0],
        [0, 1, 1, 0, 0, 0],
        [0, 0, 0, 1, 1, 0],
        [0, 1, 0, 0, 1, 0]
      ];
      var covered4 = [
        [-1, 1, 0, 1, 1, 1],
        [-1, 1, 1, 0, 1, 1],
        [-1, 1, 1, 1, 0, 1],
        [-1, 1, 1, 1, 1, 0],
        [0, 1, 1, 1, 1, -1],
        [1, 0, 1, 1, 1, -1],
        [1, 1, 0, 1, 1, -1],
        [1, 1, 1, 0, 1, -1]
      ];
      var covered3 = [
        [-1, 1, 1, 1, 0, 0],
        [-1, 1, 1, 0, 1, 0],
        [-1, 1, 0, 1, 1, 0],
        [0, 0, 1, 1, 1, -1],
        [0, 1, 0, 1, 1, -1],
        [0, 1, 1, 0, 1, -1],
        [-1, 1, 0, 1, 0, 1, -1],
        [-1, 0, 1, 1, 1, 0, -1],
        [-1, 1, 1, 0, 0, 1, -1],
        [-1, 1, 0, 0, 1, 1, -1]
      ];*/
    
    var win = Array(
      Array(Option(true), Option(true), Option(true), Option(true), Option(true)))
    var unCovered4 = Array(
      Array(null, Option(true), Option(true), Option(true), Option(true), null))
    var unCovered3 = Array(
      Array(null, Option(true), Option(true), Option(true), null, null),
      Array(null, null, Option(true), Option(true), Option(true), null),
      Array(null, Option(true), null, Option(true), Option(true), null),
      Array(null, Option(true), Option(true), null, Option(true), null))
    var unCovered2 = Array(
      Array(null, null, Option(true), Option(true), null, null),
      Array(null, Option(true), null, Option(true), null, null),
      Array(null, null, Option(true), null, Option(true), null),
      Array(null, Option(true), Option(true), null, null, null),
      Array(null, null, null, Option(true), Option(true), null),
      Array(null, Option(true), null, null, Option(true), null))
    var covered4 = Array(
      Array(Option(false), Option(true), null, Option(true), Option(true), Option(true)),
      Array(Option(false), Option(true), Option(true), null, Option(true), Option(true)),
      Array(Option(false), Option(true), Option(true), Option(true), null, Option(true)),
      Array(Option(false), Option(true), Option(true), Option(true), Option(true), null),
      Array(null, Option(true), Option(true), Option(true), Option(true), Option(false)),
      Array(Option(true), null, Option(true), Option(true), Option(true), Option(false)),
      Array(Option(true), Option(true), null, Option(true), Option(true), Option(false)),
      Array(Option(true), Option(true), Option(true), null, Option(true), Option(false)))
    var covered3 = Array(
      Array(Option(false), Option(true), Option(true), Option(true), null, null),
      Array(Option(false), Option(true), Option(true), null, Option(true), null),
      Array(Option(false), Option(true), null, Option(true), Option(true), null),
      Array(null, null, Option(true), Option(true), Option(true), Option(false)),
      Array(null, Option(true), null, Option(true), Option(true), Option(false)),
      Array(null, Option(true), Option(true), null, Option(true), Option(false)),
      Array(Option(false), Option(true), null, Option(true), null, Option(true), Option(false)),
      Array(Option(false), null, Option(true), Option(true), Option(true), null, Option(false)),
      Array(Option(false), Option(true), Option(true), null, null, Option(true), Option(false)),
      Array(Option(false), Option(true), null, null, Option(true), Option(true), Option(false)))

    def getCandidates(board: Array[Array[Option[Boolean]]]): Array[(Int, Int)] = {
      val rowCount = board.length
      val columnCount = if (board.isEmpty) 0 else board(0).length

      val candidates = new ArrayBuffer[(Int, Int)] //set of candidates
      val nonAvailableElems = new ArrayBuffer[(Int, Int)] //set of non available movements
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
      candidates.toArray
    }

    def getStateAfterMove(board: Array[Array[Option[Boolean]]], move: (Int, Int), player: Boolean) = {
      val rowCount = board.length
      val columnCount = if (board.isEmpty) 0 else board(0).length
      val cloneBoard: Array[Array[Option[Boolean]]] = Array.fill(rowCount, columnCount)(null)
      for (i <- 0 until rowCount)
        for (j <- 0 until columnCount)
          cloneBoard(i)(j) = board(i)(j)
      cloneBoard(move._1)(move._2) = Option(player)
      cloneBoard
    }

    def calculateValue(node: Node, hasBlock: Boolean): Int = {
      var point = 0
      val bufferMove = new ArrayBuffer[Array[Option[Boolean]]]()
      bufferMove.append(Node.getRow(node.board, node.move))
      bufferMove.append(Node.getColumn(node.board, node.move, node.rowCount))
      bufferMove.append(Node.getLTR(node.board, node.move, node.rowCount, node.columnCount))
      bufferMove.append(Node.getRTL(node.board, node.move, node.rowCount, node.columnCount))

      val arrayMove = bufferMove.toArray

      for (num <- node.numInARowNeeded to 1 by -1) {
        arrayMove.foreach(row => {
          val side = Node.nInARow(num, row, hasBlock)
          if (side == Option(true)) // Me
            point += WIN_VALUE / ((node.numInARowNeeded + 1 - num) * DIVIDE_RATIO)
          else if (side == Option(false)) // My Opponent
            point -= WIN_VALUE / ((node.numInARowNeeded + 1 - num) * DIVIDE_RATIO)
        })
      }

      return point
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

    def checkWinAtState(node: Node, hasBlock: Boolean): Option[Boolean] = {
      if (node.move != null) {
        val row = nInARow(node.numInARowNeeded, getRow(node.board, node.move), hasBlock)
        val column = nInARow(node.numInARowNeeded, getColumn(node.board, node.move, node.rowCount), hasBlock)
        val ltr = nInARow(node.numInARowNeeded, getLTR(node.board, node.move, node.rowCount, node.columnCount), hasBlock)
        val rtl = nInARow(node.numInARowNeeded, getRTL(node.board, node.move, node.rowCount, node.columnCount), hasBlock)

        if (row != null) return row
        else if (column != null) return column
        else if (ltr != null) return ltr
        else if (rtl != null) return rtl
        else return null
      } else return null
    }
  }

  var root: Node = null

  def setRootNode(board: Array[Array[Option[Boolean]]], player: Boolean, move: (Int, Int)) {
    root = new Node(0, player, move, board.clone())
  }

  private def minimize(node: Node, depth: Int, alpha: Int, beta: Int, hasBlock: Boolean): Int = {
    if (Node.checkWinAtState(node, hasBlock) != null || depth == 0) return Node.calculateValue(node, hasBlock) // calculate node value
    var newBeta = beta
    Node.getCandidates(node.board).foreach(move => {
      var child = new Node(node.level + 1, !node.player, move, Node.getStateAfterMove(node.board, move, !node.player))
      newBeta = math.min(newBeta, maximize(child, depth - 1, alpha, newBeta, hasBlock))
      if (alpha >= newBeta) {
        return alpha
      }
    })
    newBeta
  }

  private def maximize(node: Node, depth: Int, alpha: Int, beta: Int, hasBlock: Boolean): Int = {
    if (Node.checkWinAtState(node, hasBlock) != null || depth == 0) return Node.calculateValue(node, hasBlock) // calculate node value
    var newAlpha = alpha
    Node.getCandidates(node.board).foreach(move => {
      var child = new Node(node.level + 1, !node.player, move, Node.getStateAfterMove(node.board, move, !node.player))
      newAlpha = math.max(newAlpha, minimize(child, depth - 1, newAlpha, beta, hasBlock))
      if (newAlpha >= beta) {
        return beta
      }
    })
    newAlpha
  }

  def evaluateTreeWithAlphaBeta(board: Array[Array[Option[Boolean]]], numberOfLevel: Int, hasBlock: Boolean): (Int, Int) = {
    var maxValue = Int.MinValue
    var maxMove: (Int, Int) = (0, 0)
    //minimize(root, numberOfLevel, Int.MinValue, Int.MaxValue, hasBlock)
    Node.getCandidates(board).foreach(move => {
      val tree = new MinimaxTree
      tree.setRootNode(Node.getStateAfterMove(board, move, true), true, move)
      val newValue = tree.maximize(tree.root, numberOfLevel, Int.MinValue, Int.MaxValue, hasBlock)
      if (maxValue < newValue) {
        maxValue = newValue
        maxMove = move
      }
    })
    return maxMove
  }
}

