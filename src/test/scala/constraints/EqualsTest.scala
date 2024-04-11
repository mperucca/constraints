package constraints

import org.scalatest.funsuite.AnyFunSuite

class EqualsTest extends AnyFunSuite {

  test("Equals"):
    ComputeTest[1 == 1](true)
    ComputeTest[1 == 2](false)
    ComputeTest["a" == "a"](true)
    ComputeTest[1 == 1d](true)
    ComputeTest[1 == Unit](false)
  
  test("NaN"):
    ComputeTest[Equals[Double.NaN.type, Double.NaN.type]](false)
    ComputeTest[Not[Equals[Double.NaN.type, Double.NaN.type]]](true)
    ComputeTest[Not[Not[Equals[Double.NaN.type, Double.NaN.type]]]](false)

}
