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

  test("inline (true implies true)"):
    InlinableTest[true implies true]: Some[true]

  test("inline (true implies false)"):
    InlinableTest[true implies false]: Some[false]

  test("inline (false implies true)"):
    InlinableTest[false implies true]: Some[true]

  test("inline (false implies false)"):
    InlinableTest[false implies false]: Some[true]

  test("inline (unknown implies true)"):
    InlinableTest[Boolean implies true]: Some[true]

  test("inline (unknown implies false)"):
    InlinableTest[Boolean implies false]: None.type

  test("inline (true implies unknown)"):
    InlinableTest[true implies Boolean]: None.type

  test("inline (false implies unknown)"):
    InlinableTest[false implies Boolean]: Some[true]

  test("inline (unknown implies unknown)"):
    InlinableTest[Boolean implies Boolean]: None.type
