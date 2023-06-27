package constraints

import org.scalatest.funsuite.AnyFunSuite

class xnorTest extends AnyFunSuite:

  test("compute (true xnor true)"):
    ComputeTest[true xnor true](true)

  test("compute (true xnor false)"):
    ComputeTest[true xnor false](false)

  test("compute (false xnor true)"):
    ComputeTest[false xnor true](false)

  test("compute (false xnor false)"):
    ComputeTest[false xnor false](true)

  test("inline (true xnor true)"):
    InlinableTest[true xnor true]: Some[true]

  test("inline (true xnor false)"):
    InlinableTest[true xnor false]: Some[false]

  test("inline (false xnor true)"):
    InlinableTest[false xnor true]: Some[false]

  test("inline (false xnor false)"):
    InlinableTest[false xnor false]: Some[true]

  test("inline (unknown xnor true)"):
    InlinableTest[Boolean xnor true]: None.type

  test("inline (unknown xnor false)"):
    InlinableTest[Boolean xnor false]: None.type

  test("inline (true xnor unknown)"):
    InlinableTest[true xnor Boolean]: None.type

  test("inline (false xnor unknown)"):
    InlinableTest[false xnor Boolean]: None.type

  test("inline (unknown xnor unknown)"):
    InlinableTest[Boolean xnor Boolean]: None.type
