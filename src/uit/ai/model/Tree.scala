package uit.ai.model

class Tree[A] {
  class Node(val data: A, val children: Seq[Node])

  private val root: Node = null

  def preorder(visit: A => Unit) {
    def recur(n: Node) {
      visit(n.data)
      for (c <- n.children) recur(c)
    }
    recur(root)
  }

  def postorder(visit: A => Unit) {
    def recur(n: Node) {
      for (c <- n.children) recur(c)
      visit(n.data)
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