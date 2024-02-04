package constraints

import org.scalatest.funsuite.AnyFunSuite

class orTest extends AnyFunSuite:

  test("compute is lazy"):
    type B
    given Compute.Typed[B, Nothing] = Compute(fail("shouldn't be called"))
    ComputeTest[true or B](true)

  test("compute (true or true)"):
    ComputeTest[true or true](true)

  test("compute (true or false)"):
    ComputeTest[true or false](true)

  test("compute (false or true)"):
    ComputeTest[false or true](true)

  test("compute (false or false)"):
    ComputeTest[false or false](false)
