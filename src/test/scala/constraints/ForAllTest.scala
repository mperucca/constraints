package constraints

import org.scalatest.funsuite.AnyFunSuite

class ForAllTest extends AnyFunSuite:

  test("ForAll"):
    type A
    type B
    type C
    type I[_]
    summon[ForAll[(A, B, C), I] =:= (I[A] and (I[B] and (I[C] and true)))]
