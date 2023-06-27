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

  test("inline (true nor true)"):
    InlinableTest[true nor true]: Some[false]

  test("inline (true nor false)"):
    InlinableTest[true nor false]: Some[false]

  test("inline (false nor true)"):
    InlinableTest[false nor true]: Some[false]

  test("inline (false nor false)"):
    InlinableTest[false nor false]: Some[true]

  test("inline (unknown nor true)"):
    InlinableTest[Boolean nor true]: Some[false]

  test("inline (unknown nor false)"):
    InlinableTest[Boolean nor false]: None.type

  test("inline (true nor unknown)"):
    InlinableTest[true nor Boolean]: Some[false]

  test("inline (false nor unknown)"):
    InlinableTest[false nor Boolean]: None.type

  test("inline (unknown nor unknown)"):
    InlinableTest[Boolean nor Boolean]: None.type
