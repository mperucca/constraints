package constraints

import org.scalatest.funsuite.AnyFunSuite

class xorTest extends AnyFunSuite:

  test("compute (true xor true)"):
    ComputeTest[true xor true](false)

  test("compute (true xor false)"):
    ComputeTest[true xor false](true)

  test("compute (false xor true)"):
    ComputeTest[false xor true](true)

  test("compute (false xor false)"):
    ComputeTest[false xor false](false)

  test("inline (true xor true)"):
    InlinableTest[true xor true]: Some[false]

  test("inline (true xor false)"):
    InlinableTest[true xor false]: Some[true]

  test("inline (false xor true)"):
    InlinableTest[false xor true]: Some[true]

  test("inline (false xor false)"):
    InlinableTest[false xor false]: Some[false]

  test("inline (unknown xor true)"):
    InlinableTest[Boolean xor true]: None.type

  test("inline (unknown xor false)"):
    InlinableTest[Boolean xor false]: None.type

  test("inline (true xor unknown)"):
    InlinableTest[true xor Boolean]: None.type

  test("inline (false xor unknown)"):
    InlinableTest[false xor Boolean]: None.type

  test("inline (unknown xor unknown)"):
    InlinableTest[Boolean xor Boolean]: None.type
