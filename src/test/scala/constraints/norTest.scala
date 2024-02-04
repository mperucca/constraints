package constraints

import org.scalatest.funsuite.AnyFunSuite

class norTest extends AnyFunSuite:

  test("compute is lazy"):
    type B
    given Compute.Typed[B, Nothing] = Compute(fail("shouldn't be called"))
    ComputeTest[true nor B](false)

  test("compute (true nor true)"):
    ComputeTest[true nor true](false)

  test("compute (true nor false)"):
    ComputeTest[true nor false](false)

  test("compute (false nor true)"):
    ComputeTest[false nor true](false)

  test("compute (false nor false)"):
    ComputeTest[false nor false](true)
