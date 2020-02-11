sealed abstract class IntTree
case class Cons(root: Int, left: IntTree, right: IntTree) extends IntTree
case class Nil() extends IntTree

val T = Cons(3, Cons(6, Nil(), Nil()), Cons(21, Nil(), Nil()))


def forAll(tree: IntTree, prop: Int => Boolean) : Boolean = tree match {
  case Nil() => true
  case Cons(v, left, right) => prop(v) && forAll(left, prop) && forAll(right, prop)
}

def exists(tree: IntTree, prop: Int => Boolean) : Boolean = tree match {
  case Nil() => false
  case Cons(v, left, right) => prop(v) || exists(left, prop) || exists(right, prop)
}

def and(propL: Int => Boolean, propR: Int => Boolean): Int => Boolean = {
  a => propL(a) && propR(a)
}

def or(propL: Int => Boolean, propR: Int => Boolean): Int => Boolean = {
  a => propL(a) || propR(a)
}

def not(prop: Int => Boolean): Int => Boolean = {
  a => !prop(a)
}

def eventually(tree: IntTree, pred: Int => Boolean): Boolean = tree match {
  case Nil() => false
  case Cons(v, Cons(v2, _, _), Nil()) => pred(v) && pred(v2)
  case Cons(v, Nil(), Cons(v2, _, _)) => pred(v) && pred(v2)
  case Cons(v, Cons(v2, _,_), Cons(v3,_,_)) => pred(v) && pred(v2) || (pred(v) && pred(v3))
  case Cons(_, l, r) => eventually(l, pred) || eventually(r, pred)
}

def always(tree: IntTree, prop: Int => Boolean): Boolean = tree match {
  case Nil() => false
  case Cons(v, Cons(v2, _, _), Nil()) => prop(v) && prop(v2)
  case Cons(v, Nil(), Cons(v2, _, _)) => prop(v) && prop(v2)
  case Cons(v, Cons(v2, _,_), Cons(v3,_,_)) => prop(v) && prop(v2) && (prop(v) && prop(v3))
  case Cons(_, l, r) => always(l, prop) && always(r, prop)
}


var R3 = forAll(T, e => e % 2 == 0)
var R4 = exists(T, e => e % 7 == 0)

var R5 = eventually(T, a => a % 6 == 0 || a % 21 == 0) // Not branches
var R6 = eventually(T, a => a * 8 == 24 || a + 3 == 24) // branches

val cond1 = or(a => a % 7 == 0, a => a % 6 == 0)
val cond2 = or(a => a % 7 == 0, a => a % 3 == 0)
var R7 = always(T, cond1)
var R8 = always(T, cond2)