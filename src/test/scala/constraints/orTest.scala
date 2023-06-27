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

  test("inline (true or true)"):
    InlinableTest[true or true]: Some[true]

  test("inline (true or false)"):
    InlinableTest[true or false]: Some[true]

  test("inline (false or true)"):
    InlinableTest[false or true]: Some[true]

  test("inline (false or false)"):
    InlinableTest[false or false]: Some[false]

  test("inline (unknown or true)"):
    InlinableTest[Boolean or true]: Some[true]

  test("inline (unknown or false)"):
    InlinableTest[Boolean or false]: None.type

  test("inline (true or unknown)"):
    InlinableTest[true or Boolean]: Some[true]

  test("inline (false or unknown)"):
    InlinableTest[false or Boolean]: None.type

  test("inline (unknown or unknown)"):
    InlinableTest[Boolean or Boolean]: None.type
