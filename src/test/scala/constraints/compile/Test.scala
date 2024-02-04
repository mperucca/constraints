package constraints.compile

import constraints.Guarantee
import org.scalatest.funsuite.AnyFunSuite

class Test extends AnyFunSuite {

  test("verifyAtCompileTime true"):
    type A
    given Inlinable[A] with
      override type Result = true
      override inline def reduce: Some[true] = Some(true)
    Guarantee[A]

  test("verifyAtCompileTime false"):
    type A
    given Inlinable[A] with
      override type Result = false
      override inline def reduce: Some[false] = Some(false)
    assertDoesNotCompile("Guarantee.verifyAtCompileTime[A]")

  test("verifyAtCompileTime unknown"):
    type A
    given Inlinable[A] with
      override type Result = Nothing
      override inline def reduce: None.type = None
    assertDoesNotCompile("Guarantee.verifyAtCompileTime[A]")

}
