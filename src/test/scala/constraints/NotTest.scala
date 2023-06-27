package constraints

import org.scalatest.funsuite.AnyFunSuite

class NotTest extends AnyFunSuite:

  test("compute Not[true]"):
    ComputeTest[Not[true]](false)

  test("compute Not[false]"):
    ComputeTest[Not[false]](true)

  test("inline Not[true]"):
    InlinableTest[Not[true]]: Some[false]

  test("inline Not[false]"):
    InlinableTest[Not[false]]: Some[true]

  test("inline Not[unknown]"):
    InlinableTest[Not[Boolean]]: None.type
