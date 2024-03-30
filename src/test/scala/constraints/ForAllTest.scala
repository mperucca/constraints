package constraints

import org.scalatest.funsuite.AnyFunSuite

class ForAllTest extends AnyFunSuite:

  test("ForAll"):
    def m[A, B, C, I[_]] =
      summon[ForAll[(A, B, C), I] =:= (I[A] and (I[B] and (I[C] and true)))]
