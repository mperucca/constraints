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

  test("inline (true and true)"):
    InlinableTest[true and true]: Some[true]

  test("inline (true and false)"):
    InlinableTest[true and false]: Some[false]

  test("inline (false and true)"):
    InlinableTest[false and true]: Some[false]

  test("inline (false and false)"):
    InlinableTest[false and false]: Some[false]

  test("inline (unknown and true)"):
    InlinableTest[Boolean and true]: None.type

  test("inline (unknown and false)"):
    InlinableTest[Boolean and false]: Some[false]

  test("inline (true and unknown)"):
    InlinableTest[true and Boolean]: None.type

  test("inline (false and unknown)"):
    InlinableTest[false and Boolean]: Some[false]

  test("inline (unknown and unknown)"):
    InlinableTest[Boolean and Boolean]: None.type
