package constraints

import org.scalatest.funsuite.AnyFunSuite

class nandTest extends AnyFunSuite:

  test("compute is lazy"):
    type B
    given Compute.Typed[B, Nothing] = Compute(fail("shouldn't be called"))
    ComputeTest[false nand B](true)

  test("compute (true nand true)"):
    ComputeTest[true nand true](false)

  test("compute (true nand false)"):
    ComputeTest[true nand false](true)

  test("compute (false nand true)"):
    ComputeTest[false nand true](true)
