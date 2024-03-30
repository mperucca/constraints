package constraints

import constraints.compile.*
import org.scalatest.funsuite.AnyFunSuite

class GuaranteeTest extends AnyFunSuite:

  test("trust"):
    type A
    Guarantee.trust[A]

  test("testAtRuntime true"):
    type A
    given Compute[A] with
      override type Result = Boolean
      override def compute: Boolean = true
    val guarantee: Either[Guarantee[Not[A]], Guarantee[A]] = Guarantee.test
    assert(guarantee.isRight)

  test("testAtRuntime false"):
    type A
    given Compute[A] with
      override type Result = Boolean
      override def compute: Boolean = false
    val guarantee: Either[Guarantee[Not[A]], Guarantee[A]] = Guarantee.test
    assert(guarantee.isLeft)

  test("and"):
    type A
    type B
    val guarantee1: Guarantee[A] = Guarantee.trust
    val guarantee2: Guarantee[B] = Guarantee.trust
    guarantee1 and guarantee2: Guarantee[A and B]
