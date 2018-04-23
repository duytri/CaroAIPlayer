package uit.ai.model

import java.util.Random

class CaroPlayer extends Player {
  def getName: String = "Computer AI"
  def nextMoveRandom(board: Array[Array[Cell]], playerSide: Cell): (Int, Int) = {
    val size = board.length
    val random = new Random()
    while (true) {
      val i = random.nextInt(size)
      val j = random.nextInt(size)
      if (board(i)(j) == Blank)
        return (i, j)
    }
    (-1, -1)
  }
  def weightedRandom(max: Int, numDice: Int): Int = {
    var num = 0
    val random = new Random
    for (i <- 0 until numDice) {
      num += random.nextInt(max / numDice) * (max / numDice)
    }
    return num
  }

  def nextMove(board: Array[Array[Cell]], playerSide: Cell): (Int, Int) = {
    val size = board.length
    var boardTree = new Tree[State]()
    (-1, -1)
  }
}