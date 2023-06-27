package constraints

import org.scalatest.funsuite.AnyFunSuite

class GuaranteedTest extends AnyFunSuite:

  class V
  type C[V]
  def check(value: V)(guarantee: Guarantee[C[value.type]]): Unit = ()

  test("base trait"):
    val guaranteed = new Guaranteed[V]:
      override val value: V = new V
      override def guarantee: Guarantee[C[value.type]] = Guarantee.trust

    check(guaranteed.value)(guaranteed.guarantee)

  test("value trait"):
    val guaranteed = new Guaranteed.Value(new V):
      override def guarantee: Guarantee[C[value.type]] = Guarantee.trust

    check(guaranteed.value)(guaranteed.guarantee)
    assertTypeError("check(new V)(guaranteed.guarantee)")

  test("type trait"):
    val v = new V
    val guaranteed = new Guaranteed.Type[v.type]:
      override def guarantee: Guarantee[C[value.type]] = Guarantee.trust

    check(v)(guaranteed.guarantee)
    assertTypeError("check(new V)(guaranteed.guarantee)")

  test("apply"):
    val v = new V
    val guaranteed = Guaranteed(v, Guarantee.trust[C[v.type]])

    check(v)(guaranteed.guarantee)
    assertTypeError("check(new V)(guaranteed.guarantee)")

  test("Refined"):
    val v = new V
    val guaranteed = Guaranteed.Refined[C](v)(Guarantee.trust)

    check(v)(guaranteed.guarantee)
    assertTypeError("check(new V)(guaranteed.guarantee)")
