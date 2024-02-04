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
