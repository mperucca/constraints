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

  test("compute (false nand false)"):
    ComputeTest[false nand false](true)

  test("inline (true nand true)"):
    InlinableTest[true nand true]: Some[false]

  test("inline (true nand false)"):
    InlinableTest[true nand false]: Some[true]

  test("inline (false nand true)"):
    InlinableTest[false nand true]: Some[true]

  test("inline (false nand false)"):
    InlinableTest[false nand false]: Some[true]

  test("inline (unknown nand true)"):
    InlinableTest[Boolean nand true]: None.type

  test("inline (unknown nand false)"):
    InlinableTest[Boolean nand false]: Some[true]

  test("inline (true nand unknown)"):
    InlinableTest[true nand Boolean]: None.type

  test("inline (false nand unknown)"):
    InlinableTest[false nand Boolean]: Some[true]

  test("inline (unknown nand unknown)"):
    InlinableTest[Boolean nand Boolean]: None.type
