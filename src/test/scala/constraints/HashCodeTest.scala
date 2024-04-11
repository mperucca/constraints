package constraints

import org.scalatest.funsuite.AnyFunSuite

class HashCodeTest extends AnyFunSuite {

  test("HashCode"):
    ComputeTest[##["abc"]]("abc".hashCode())
    ComputeTest[##[1]](1)

}
