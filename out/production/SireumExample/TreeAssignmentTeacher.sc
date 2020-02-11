object TreeAssignmentTeacher {
  sealed abstract class IntTree
  case class Cons(root: Int, left: IntTree, right: IntTree) extends IntTree
  case class Nil() extends IntTree

  val T = Cons(3, Cons(6, Nil(), Nil()), Cons(21, Nil(), Nil()))

  def addAll(tree : IntTree): Int = tree match {
    case Nil() => 0
    case Cons(root, left, right) => root + addAll(left) + addAll(right)
  }

  def fold(tree: IntTree, seed: Int, f: (Int, Int, Int) => Int) : Int = tree match {
    case Nil() => seed
    case Cons(root, left, right) => f(root, fold(left, seed, f), fold(right, seed, f))
  }

  def addAll1(tree:IntTree) = fold(tree, 0, (x,y,z)=> x + y + z)

  def fold2(tree: IntTree, seed: Int, f:(Int,Int,Int) => Int, cont: Int => Int):Int= tree match {
    case Nil() => cont(seed)
    case Cons(root, left, right) => fold2(left, seed, f, l => fold2(right, seed, f, r => cont(f(root, l, r))))
  }

  def addAll2(tree: IntTree) = fold2(tree, 0, (x, y, z) => x + y + z, x => x)

  var R1 = addAll(T)
  var R2 = addAll1(T)
}