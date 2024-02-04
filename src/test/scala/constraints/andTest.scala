package constraints

import org.scalatest.funsuite.AnyFunSuite

class andTest extends AnyFunSuite:

  test("compute is lazy"):
    type B
    given Compute.Typed[B, Nothing] = Compute(fail("shouldn't be called"))
    ComputeTest[false and B](false)

  test("compute (true and true)"):
    ComputeTest[true and true](true)

  test("compute (true and false)"):
    ComputeTest[true and false](false)

  test("compute (false and true)"):
    ComputeTest[false and true](false)

  test("compute (false and false)"):
    ComputeTest[false and false](false)
