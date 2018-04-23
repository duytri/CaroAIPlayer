package uit.ai.model

import uit.ai.model.CaroBoard

class MinimaxTree[State] {
  class Node(
    val level: Int,
    val player: Cell,
    val isMax: Boolean,
    val move: (Int, Int),
    var value: Option[Int],
    val board: CaroBoard,
    val children: Seq[Node])

  private val root: Node = null

  def preorder(visit: CaroBoard => Unit) {
    def recur(n: Node) {
      visit(n.board)
      for (c <- n.children) recur(c)
    }
    recur(root)
  }

  def postorder(visit: CaroBoard => Unit) {
    def recur(n: Node) {
      for (c <- n.children) recur(c)
      visit(n.board)
    }
    recur(root)
  }

  def height(n: Node): Int = {
    1 + n.children.foldLeft(-1)((h, c) => h max height(c))
  }

  def size(n: Node): Int = {
    1 + n.children.foldLeft(0)((s, c) => s + size(c))
  }
  
  def generateChildren(): List[State] = {
    val children = new ListBuffer[State]
    for (e <- board.getEmpty()) {
      var state: State = null
      if (player == Square)
        state = new State(level + 1, Circle, !isMax, e, null, board.updateAndGet(e._1, e._2, Circle))
      else
        state = new State(level + 1, Square, !isMax, e, null, board.updateAndGet(e._1, e._2, Square))
      children.append(state)
    }
    children.toList
  }

  def evaluateMove(board: CaroBoard, move: (Int, Int)): Int = {
    -10000
  }
}