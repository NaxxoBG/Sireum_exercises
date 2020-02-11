object IntBinaryTree {

  sealed abstract class IntBinaryTree
  case class Leaf(v: Int) extends IntBinaryTree
  case class Node(value: Int, left: IntBinaryTree, right: IntBinaryTree) extends IntBinaryTree

  def addAllNodes(tree: IntBinaryTree): Int = {
    def helper(trees: List[IntBinaryTree], seed: Int, cont: Int => Int): Int =
      trees match {
        case Nil => cont(seed)
        case Leaf(i) :: ls => helper(ls, cont(i + seed), cont)
        case Node(i, l, r) :: ls => helper(l :: r :: ls, cont(i + seed), cont)
      }
    helper(List(tree), 0, a => a)
  }

  val ibt = Node(1, Node(2, Leaf(1), Node(3, Leaf(1), Leaf(1))), Node(4, Leaf(1), Node(5, Leaf(1), Leaf(1))))
  val ibt2 = Node(10, Leaf(1), Node(10, Leaf(1), Leaf(2)))
  val ibt3 = Leaf(2)
  val ibt4 = Node(2, Leaf(3), Leaf(4))

  val R1 = addAllNodes(ibt)
  val R2 = addAllNodes(ibt2)
  val R3 = addAllNodes(ibt3)
  val R4 = addAllNodes(ibt4)
}