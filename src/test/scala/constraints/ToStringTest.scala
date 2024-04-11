package constraints

import org.scalatest.funsuite.AnyFunSuite

class ToStringTest extends AnyFunSuite {
  
  test("ToString"):
    ComputeTest[ToString["a"]]("a")
    ComputeTest[ToString[1]]("1")
    ComputeTest[ToString[Unit]]("()")

}
