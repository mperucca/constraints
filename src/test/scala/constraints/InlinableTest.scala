package constraints

import org.scalatest.funsuite.AnyFunSuite

object InlinableTest:

  transparent inline def apply[A](using inlinable: Inlinable[A]): Option[inlinable.Result] =
    inlinable.reduce

class InlinableTest extends AnyFunSuite:

  test("Null"):
    InlinableTest[Null]: Some[Null]

  test("None"):
    InlinableTest[None.type]: Some[None.type]

  test("Some"):
    InlinableTest[Some[1]]: Some[Some[1]]