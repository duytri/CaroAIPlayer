package uit.ai.model

import scala.collection.mutable.ListBuffer

class State(
  val level: Int,
  val player: Cell,
  val isMax: Boolean,
  var value: Int,
  val board: CaroBoard) {

  def generateChildren(): List[State] = {
    val children = new ListBuffer[State]
    for (e <- board.getEmpty()) {
      var state: State = null
      if (player == Square)
        state = new State(level + 1, Circle, !isMax, null, board.updateAndGet(e._1, e._2, Circle))
      else
        state = new State(level + 1, Square, !isMax, null, board.updateAndGet(e._1, e._2, Square))
      children.append(state)
    }
    children.toList
  }

  def evaluateMove(board: CaroBoard, move: (Int, Int)): Int = {
    -10000
  }
}