package constraints

import org.scalatest.funsuite.AnyFunSuite

class ExistsTest extends AnyFunSuite:

  test("Exists"):
    type A
    type B
    type C
    type I[_]
    summon[Exists[(A, B, C), I] =:= (I[A] or (I[B] or (I[C] or false)))]
