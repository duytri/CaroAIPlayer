package uit.ai.model

import scala.collection.mutable.ListBuffer

class MinimaxTree[State] {
  class Node(
    val level: Int,
    val player: Cell,
    val isMax: Boolean,
    val isLeaf: Boolean,
    val move: (Int, Int),
    var value: Option[Int],
    val board: CaroBoard,
    val children: Seq[Node]) {

    def generateChildren() = {
      for (e <- board.getEmpty()) {
        var node: Node = null
        if (player == Square)
          node = new Node(level + 1, Circle, !isMax, false, e, null, board.updateAndGet(e._1, e._2, Circle), null)
        else
          node = new Node(level + 1, Square, !isMax, false, e, null, board.updateAndGet(e._1, e._2, Square), null)
        children.+:(node)
      }
      children
    }

    def evaluateNode() = {
      -10000
    }
  }

  private var root: Node = null

  def setRootNode(player: Cell, board: CaroBoard) {
    root = new Node(0, player, isMax = false, false, null, null, board, null)
  }

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

}