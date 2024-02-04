package constraints

import org.scalatest.funsuite.AnyFunSuite

class NotTest extends AnyFunSuite:

  test("compute Not[true]"):
    ComputeTest[Not[true]](false)

  test("compute Not[false]"):
    ComputeTest[Not[false]](true)
