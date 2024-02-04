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
