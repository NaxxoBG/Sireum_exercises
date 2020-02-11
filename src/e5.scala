import org.sireum.logika._


def square(a: ZS): Unit = {
  l"""{ modifies     a
        post         ∀i: (0 ..< a.size)  a(i) == a_in(i) * a_in(i)              }"""

  var x: Z = 0

  while (x != a.size) {
    l"""{ invariant  ∀i: (0 ..< x)  a(i) == a_in(i) * a_in(i)
                     ∀i: (x ..< a.size)  a(i) == a_in(i)
                     0 ≤ x
                     x ≤ a.size
          modifies   x, a                                                       }"""

    a(x) = a(x) * a(x)

    x = x + 1
  }
}


val xs : ZS = ZS(1,2,3,4,5)
println("Values:         ", xs)

square(xs)
println("After squaring: ", xs)
