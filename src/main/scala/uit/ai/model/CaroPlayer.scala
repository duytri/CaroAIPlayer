package main.scala.uit.ai.model

import java.util.Random
import java.lang.Math

class CaroPlayer extends Player {
  val numberOfLevel = 5

  def getName: String = "Computer AI"

  def nextMoveRandom(board: CaroBoard, playerSide: Cell, hasBlock: Boolean): (Int, Int) = {
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

  /*def nextMove(board: CaroBoard, playerSide: Cell, hasBlock: Boolean): (Int, Int) = {
    // rebuild caro board
    var countEmpty = 0
    val boolBoard: Array[Array[Option[Boolean]]] = Array.fill[Option[Boolean]](board.rowCount, board.columnCount)(null)
    val tempBoard = board.getBoard
    for (r <- 0 until board.rowCount)
      for (c <- 0 until board.columnCount) {
        if (tempBoard(r)(c) == Blank) {
          countEmpty += 1
          boolBoard(r)(c) = null
        } else if (tempBoard(r)(c) == playerSide) {
          boolBoard(r)(c) = Option(true) // True: Me, False: Opponent
        } else {
          boolBoard(r)(c) = Option(false)
        }
      }
    if (countEmpty < board.rowCount * board.columnCount) {
      val minimax = new MinimaxTree
      minimax.evaluateTreeWithAlphaBeta(boolBoard, numberOfLevel, hasBlock)
    } else {
      (board.rowCount / 2, board.columnCount / 2)
    }
  }*/

  def nextMove(board: Array[Array[Byte]], playerSide: Byte, hasBlock: Boolean): (Int, Int) = {
    val rowCount = board.length
    val columnCount = board(0).length
    val countNonEmpty = board.foldLeft(0)((x, row) => {
      x + row.foldLeft(0)((i, j) => i + Math.abs(j))
    })

    if (countNonEmpty > 0) {
      val minimax = new MinimaxTree
      minimax.evaluateTreeWithAlphaBeta(board, numberOfLevel, hasBlock)
    } else {
      (rowCount / 2, columnCount / 2)
    }
  }
}