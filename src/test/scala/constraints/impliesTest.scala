package constraints

import org.scalatest.funsuite.AnyFunSuite

class impliesTest extends AnyFunSuite:

  test("compute is lazy"):
    type B
    given Compute.Typed[B, Nothing] = Compute(fail("shouldn't be called"))
    ComputeTest[false implies B](true)

  test("compute (true implies true)"):
    ComputeTest[true implies true](true)

  test("compute (true implies false)"):
    ComputeTest[true implies false](false)

  test("compute (false implies true)"):
    ComputeTest[false implies true](true)

  test("compute (false implies false)"):
    ComputeTest[false implies false](true)
