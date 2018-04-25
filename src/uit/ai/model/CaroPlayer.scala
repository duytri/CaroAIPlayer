package uit.ai.model

import java.util.Random
import scala.Array

class CaroPlayer extends Player {
  def getName: String = "Computer AI"
  def nextMoveRandom(board: CaroBoard, playerSide: Cell): (Int, Int) = {
    val size = board.rowCount
    val b = board.getBoard
    val random = new Random()
    while (true) {
      val i = random.nextInt(size)
      val j = random.nextInt(size)
      if (b(i)(j) == Blank)
        return (i, j)
    }
    (-1, -1)
  }

  def nextMove(board: CaroBoard, playerSide: Cell): (Int, Int) = {
    // rebuild caro board
    val boolBoard: Array[Array[Option[Boolean]]] = Array.fill[Option[Boolean]](board.rowCount, board.columnCount)(null)
    val tempBoard = board.getBoard
    for (r <- 0 until board.rowCount)
      for (c <- 0 until board.columnCount) {
        if (tempBoard(r)(c) == Blank) boolBoard(r)(c) = null
        else if (tempBoard(r)(c) == playerSide) boolBoard(r)(c) = Option(true)
        else boolBoard(r)(c) = Option(false)
      }
    // build tree
    var minimaxTree = new MinimaxTree()
    minimaxTree.setRootNode(boolBoard)
    minimaxTree.fillInTheTree(5)
    (7, 7)
  }
}