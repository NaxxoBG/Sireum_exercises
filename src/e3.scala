import org.sireum.logika._


val x: Z = readInt()
val y: Z = readInt()
var max: Z = 0
if (x > y) {
  max = x
  l"""{ 1. max ≥ x  ∧  max < y     auto      }"""
} else {
  l"""{ 1. ¬(x > y)                premise
        2. y ≥ x                   algebra 1   }"""
  max = y
  l"""{ 1. max == y                premise
        2. y ≥ x                   premise
        3. max ≥ y                 algebra 1
        4. max ≥ x                 algebra 1 2
        5. max ≥ x  ∧  max ≥ y     ∧i 4 3      }"""
}
l"""{ 1. max ≥ x  ∧  max ≥ y       premise     }"""
println("Maximum of ", x, " and ", y, " is ", max, ".")
