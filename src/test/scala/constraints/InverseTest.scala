package constraints

import org.scalatest.funsuite.AnyFunSuite

class InverseTest extends AnyFunSuite:

  test("Inverse"):
    type I[_]
    type A
    summon[Inverse[I][A] =:= Not[I[A]]]
