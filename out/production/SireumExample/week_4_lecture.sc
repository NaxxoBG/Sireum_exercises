object week_4_lecture {

  def addAll(list: IntList): Int =
    list match {
      case Nil() => 0
      case Cons(head, tail) => head + addAll(tail);
    }

  def find(list: IntList, value: Int): Boolean = list match {
    case Nil() => false
    case Cons(head, tail) =>
      if (head == value) {
        true
      } else {
        find(tail, value)
      }
  }

  def foldl(list: IntList, seed: Int, func: (Int, Int) => Int): Int = list match {
    case Nil() => seed
    case Cons(head, tail) => foldl(tail, func(head, seed), func);
  }

  def addAllFold(list: IntList): Int = {
    foldl(list, 0, (a, b) => a + b)
  }

  def MultiplyAllFold(list: IntList): Int = {
    foldl(list, 1, (a, b) => a * b)
  }

  def map(list: IntList, f: Int => Int): IntList = list match {
    case Nil() => Nil();
    case Cons(head, tail) => Cons(f(head), map(tail, f));
  }

  def findl(list: IntList, value: Int): Boolean = {
    foldl(map(list, a => if (a == value) 1 else 0), 0, (c, d) => c + d) != 0
  }

  def foldr(list: IntList, seed: Int, f: (Int, Int) => Int): Int = list match {
    case Nil() => seed
    case Cons(head, tail) => f(head, foldr(tail, seed, f))
  }

  def addAll3(list: IntList):Int = foldr(list, 0, (a, b) => a+b)

  def foldrc(list: IntList, seed: Int, f: (Int, Int) => Int, cont: Int => Int) : Int =
    list match {
      case Nil()=> cont(seed)
      case Cons(head, tail) => foldrc(tail, seed, f, x => cont(f(head, x)))
    }

  def addAll4(list: IntList) : Int =
    foldrc(list, 0, (a,b) => a + b, x => x)

  def maprc(list:IntList, f: Int => Int, cont: IntList => IntList) : IntList = {
    list match {
      case Nil() => cont(Nil())
      case Cons(head, tail) => maprc(tail, f, x => cont(Cons(f(head), x)))
    }
  }

  sealed abstract class IntList

  case class Nil() extends IntList

  case class Cons(hd: Int, tl: IntList) extends IntList

  val L = Cons(1, Cons(2, Nil()))
  val R1 = addAll(L)
  val R2 = addAll(Cons(7, L))
  val R3 = foldl(L, 0, (a, b) => a + b)
  val R4 = MultiplyAllFold(L)
  val R5 = findl(L, 3)
  val R6 = findl(L, 2)
  val R7 = addAll4(L)
  val R8 = maprc(L, x => x+10, x => x)


  sealed abstract class LazyIntList

  case class LCons(hd: Int, tl: () => LazyIntList) extends  LazyIntList
  case class LNil() extends LazyIntList

  def lazyNil(): LazyIntList = {
    LNil()
  }

  def lazyCons(hd: Int, list: LazyIntList): LazyIntList = {
    LCons(hd, () => list)
  }

  def lazyFold(list: LazyIntList, seed : Int, f:(Int, Int) => Int) : Int = list match {
    case LNil() => return seed
    case LCons(head, tail) => lazyFold(tail(),f(head, seed), f)
  }

  def addAll6(list: LazyIntList) = lazyFold(list, 0, (a, b) => a + b)

  val LL = lazyCons(1, lazyCons(2, lazyNil()))

  val R9 = addAll6(LL)
  val R10 = addAll6(lazyCons(7, LL))


  def lazyRange(from: Int, to: Int): LazyIntList = {
    if (from <= to) {
      LCons(from, () => lazyRange(from+1 ,to))
    } else {
      LNil()
    }
  }

  val R11 = addAll6(lazyRange(1,3))
}