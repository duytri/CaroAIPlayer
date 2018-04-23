package uit.ai.model

import scala.collection.mutable.ListBuffer

class CaroBoard(board: Array[Array[Cell]], hasBlock: Boolean) {

  def this(rows: Int, cols: Int, hasBlock: Boolean) = this(Array.fill(rows, cols)(Blank): Array[Array[Cell]], hasBlock)
  def this(size: Int, hasBlock: Boolean) = this(size, size, hasBlock)

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

  def getBoard = board

  def getEmpty(): List[(Int, Int)] = {
    val result = new ListBuffer[(Int, Int)]
    for (i <- 0 until rowCount)
      for (j <- 0 until columnCount)
        if (board(i)(j) == Blank) result.append((i, j))
    result.toList
  }

  // get board as collection of rows
  def rows: Seq[Array[Cell]] = {
    for (r <- 0 until rowCount)
      yield board(r)
  }

  // get board as collection of columns
  def columns: Seq[Array[Cell]] = {
    for (c <- 0 until columnCount) yield (
      for (r <- (0 until rowCount))
        yield board(r)(c)).toArray
  }

  //get board as collection of diagonals from left to right
  def diagonalsLTR: Seq[Array[Cell]] = {
    for (offset <- (1 - columnCount) until columnCount) yield (
      for (row <- 0 until rowCount if offset + row < columnCount && offset + row > -1)
        yield (board(row)(row + offset))).toArray
  }

  //get board as collection of diagonals from right to left
  def diagonalsRTL: Seq[Array[Cell]] = {
    for (offset <- 0 until rowCount + rowCount - 1) yield (
      for (col <- 0 until columnCount if offset - col < rowCount && offset - col > -1)
        yield (board(offset - col)(col))).toArray
  }

  // find the winner
  def determineWinner: GameResult.Value = {
    val checkForWinner = { array: Array[Cell] =>
      CaroBoard.nInARow(numInARowNeeded, array, hasBlock) match {
        case Some(player) => return player match { // non-local return!
          case Square => GameResult.Square
          case Circle => GameResult.Circle
          case other => throw new Exception("Error, '" + other + "' is not a player.")
        }
        case None => // do nothing
      }
    }

    rows foreach checkForWinner
    columns foreach checkForWinner
    diagonalsLTR foreach checkForWinner
    diagonalsRTL foreach checkForWinner

    if (board.map(row => row.contains(Blank)).contains(true)) {
      return GameResult.NoResult
    }

    return GameResult.Tie
  }

  // neu toa do chua vuot ra ngoai ban co va vi tri do dang BLANK
  def validMove(row: Int, col: Int): Boolean = {
    return row < rowCount && row >= 0 && col < columnCount && col >= 0 && board(row)(col) == Blank
  }

  // cap nhat lai nuoc di nguoi choi Cell danh
  def update(row: Int, col: Int, Cell: Cell) = {
    board(row)(col) = Cell
  }
  
  // cap nhat lai nuoc di nguoi choi Cell danh
  def updateAndGet(row: Int, col: Int, Cell: Cell) = {
    board(row)(col) = Cell
    this
  }
}

object CaroBoard {

  def threeInARow(list: List[Cell]): Option[Cell] = list match {
    case Nil => None
    case x :: y :: z :: tail if x == y && y == z && z != Blank => Some(z)
    case _ :: tail => threeInARow(tail)
  }

  def nInARow(n: Int, array: Array[Cell], hasBlock: Boolean): Option[Cell] = {
    for (i <- 0 until array.length - (n - 1)) {
      var allTrue = true;
      for (j <- i + 1 until i + n) {
        allTrue &= array(j - 1) == array(j)
      }
      if (allTrue && array(i) != Blank) {
        if (hasBlock) {
          if (i > 0 && i + n < array.length && array(i - 1) != array(i) && array(i + n) != array(i) && array(i - 1) != Blank && array(i + n) != Blank) // dieu kien chan hai dau thi khong thang
            return None
          else
            return Some(array(i))
        } else
          return Some(array(i))
      }
    }

    return None
  }

}