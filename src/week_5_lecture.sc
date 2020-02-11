object week_5_lecture {
  sealed abstract class IntList
  case class Cons(hd: Int, tl: IntList) extends IntList
  case class Nil() extends IntList

  def range(m:Int, n:Int):IntList = {
    if (m <= n) {
      Cons(m, range(m+1,n))
    } else {
      Nil()
    }
  }

  val R1 = range(0, 4)

  def point(n:Int) = {
    if (0 < n) {
      range(0, n)
    } else {
      range(n, 0)
    }
  }

  def concat(list1:IntList, list2:IntList) : IntList = list1 match {
    case Nil() => list2
    case Cons(h, tl) => Cons(h, concat(tl, list2))
  }

  val R2 = concat(range(0, 2), range(3, 5))

  def andL(propL: Int => Boolean, propR: Int => Boolean) : Int => Boolean = {
    a => propL(a) && propR(a)
  }

  def orL(propL: Int => Boolean, propR: Int => Boolean) : Int => Boolean = {
    a => propL(a) || propR(a)
  }

  def forAll(list: IntList, prop: Int=>Boolean):Boolean = list match {
    case Nil() => true
    case Cons(hd, tl) => prop(hd) && forAll(tl, prop)
  }

  def notL(prop: Int => Boolean): Int => Boolean = {
      a => !prop(a)
  }

  def existsL(list: IntList, prop:Int => Boolean) : Boolean = list match {
    case Nil() => false
    case Cons(hd, tl) => prop(hd) || existsL(tl, prop)
  }

  def impliesL(propL: Int => Boolean, propR: Int => Boolean) : Int => Boolean = {
    a => !propL(a) || propR(a)
  }

  val R3 = existsL(range(1,2), a => a % 3 == 0)


  val ls = Cons(1, Cons(7, Cons(3, Nil())))
  val ls2 = concat(Cons(1, Cons(7, Nil())), Cons(2, Cons(3, Nil())))
  val p1 = (x:Int) => x > 3
  val p2 = (x:Int) => x < 10
  val tp1 = p1(1)


  sealed abstract class Prop
  case class BoolProp(b:Boolean) extends Prop
  case class PredProp(f:Int=> Prop) extends Prop

  def propVal(prop:Prop, list: IntList):Prop = (prop,list) match {
    case (BoolProp(_), _)=> prop
    case (PredProp(f), Cons(h, t))=>propVal(f(h), t)
    case (_,_)=>prop
  }

  val P1 = PredProp(x => PredProp(y=>BoolProp(x < y)))
  val P2 = PredProp(_ => PredProp(y=>BoolProp(1 < y)))

  val P1_R1 = propVal(P1, Cons(1, Cons(2, Nil())))
  val P1_R2 = propVal(P2, Cons(1, Cons(2, Nil())))


  def and(propL: Prop, propR: Prop):Prop = (propL, propR) match {
    case (BoolProp(x), BoolProp(y)) => BoolProp(x && y)
    case (PredProp(f), PredProp(g))=> PredProp(a => and(f(a), g(a)))
    case (BoolProp(_), PredProp(g)) => PredProp(a => and(propL, g(a)))
    case (PredProp(f), BoolProp(_)) => PredProp(a=>and(f(a), propR))
  }

  val R4 = propVal(and(P1, P2), Cons(2, Cons(1, Nil())))

  def forall(list:IntList, prop:Prop):Prop = (list, prop) match {
    case (Nil(), _) => BoolProp(true)
    case (Cons(head, tail), PredProp(f)) => and(f(head), forall(tail, prop))
    case (_, _) => prop
  }

  val T1 = forall(Nil(), BoolProp(false))

  def or(propL: Prop, propR: Prop):Prop = (propL, propR) match {
    case (BoolProp(x), BoolProp(y)) => BoolProp(x || y)
    case (PredProp(f), PredProp(g))=> PredProp(a => or(f(a), g(a)))
    case (BoolProp(_), PredProp(g)) => PredProp(a => or(propL, g(a)))
    case (PredProp(f), BoolProp(_)) => PredProp(a=>or(f(a), propR))
  }

  val P4 = or(BoolProp(1 < 2), BoolProp(2 < 1))

  def not(prop:Prop):Prop = prop match {
    case BoolProp(b)=> BoolProp(!b)
    case PredProp(f) => PredProp(x => not(f(x)))
  }

  def implies(propL: Prop, propR: Prop):Prop = or(not(propL), propR)

  def exists(list:IntList, prop: Prop):Prop = (list, prop) match {
    case (Nil(), _) => BoolProp(false)
    case (Cons(head, tail), PredProp(f)) => or(f(head), exists(tail, prop))
    case (_, _) => prop
  }

  def head(list: IntList):Int = list match {
    case Nil() => 0
    case Cons(hd, _) => hd
  }

  def sum(list: IntList, tSum:Int):Int = list match {
    case Nil() => tSum
    case Cons(hd, tl) => sum(tl, hd + tSum)
  }

  val F1 = PredProp(x => BoolProp(x == sum(range(1, 3), 0)))
  val F2 = propVal(F1, Cons(5, Nil()))

  val T2 = PredProp((x:Int) => BoolProp(x == head(Cons(x, Nil()))))
  val testT1 = propVal(T2, Cons(4, Nil()))


  val R5 = exists(Cons(1, Cons(2, Nil())), PredProp(x => BoolProp(x > 3)))

  val R6 = forall(range(1,10), PredProp(x => exists(range(1, 11), PredProp( y => BoolProp(x < y)))))

  val R7 = forall(Cons(2, Cons(5, Cons(6, Cons(7, Nil())))), PredProp(x => forall(range(1, 5), PredProp(y => or(BoolProp(x > 6), BoolProp(x * y <= 30))))))

  val testHead = head(ls)

  val F3 = PredProp(x=>PredProp(y=> BoolProp(x==sum(range(1,y), 0))))
  val G2 = PredProp(x=>PredProp(y=> BoolProp(x==(y*(y+1))/3)))
  val tpropVal = propVal(F2, Cons(6, Cons(3, Nil())))
  val tpropG2 = propVal(F3, Cons(6, Cons(3, Nil())))
  val tpropG3 = propVal(F3, Cons(3, Cons(6, Nil())))
  val tpropG4 = propVal(G2, Cons(3, Cons(6, Nil())))
  val tpropG5 = propVal(G2, Cons(6, Cons(3, Nil())))

  val tp4 = propVal(implies(F3, G2), Cons(6, Cons(3, Nil())))
  val tp5 = propVal(implies(G2, F3), Cons(6, Cons(3, Nil())))

  val F5 = forall(range(1, 10), forall(range(1, 100), F3))
  val F6 = forall(range(1, 10), exists(range(1, 100), G2))
  val F7 = forall(range(1, 10), forall(range(1, 100), implies(F3, G2)))


  def uRange(m:Int, n:Int):IntList = {
    if (m+1 <= n)
      Cons( if (m % 2 !=0) m else m + 1, uRange(m + 2, n))
    else if (m == n && m % 2 != 0)
      Cons(m, Nil())
    else
      Nil()
  }

  def uRange2(m:Int, n:Int):IntList = {
    if (m <= n)
      if (m % 2 == 0)
        uRange2(m+1, n)
      else
        Cons(m, uRange2(m+1, n))
    else
      Nil()
  }

  val testUrange1 = uRange2(6, 10)
  val testUrange2 = uRange2(2, 7)
  val testUrange3 = uRange2(3, 10)
  val testUrange4 = uRange2(3, 7)
  val testUrange5 = uRange2(3, 3)

}