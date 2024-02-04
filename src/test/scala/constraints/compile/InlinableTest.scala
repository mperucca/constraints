package constraints.compile

import constraints.*
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


  test("inline (true and true)"):
    InlinableTest[true and true]: Some[true]

  test("inline (true and false)"):
    InlinableTest[true and false]: Some[false]

  test("inline (false and true)"):
    InlinableTest[false and true]: Some[false]

  test("inline (false and false)"):
    InlinableTest[false and false]: Some[false]

  test("inline (unknown and true)"):
    InlinableTest[Boolean and true]: None.type

  test("inline (unknown and false)"):
    InlinableTest[Boolean and false]: Some[false]

  test("inline (true and unknown)"):
    InlinableTest[true and Boolean]: None.type

  test("inline (false and unknown)"):
    InlinableTest[false and Boolean]: Some[false]

  test("inline (unknown and unknown)"):
    InlinableTest[Boolean and Boolean]: None.type


  test("inline (true implies true)"):
    InlinableTest[true implies true]: Some[true]

  test("inline (true implies false)"):
    InlinableTest[true implies false]: Some[false]

  test("inline (false implies true)"):
    InlinableTest[false implies true]: Some[true]

  test("inline (false implies false)"):
    InlinableTest[false implies false]: Some[true]

  test("inline (unknown implies true)"):
    InlinableTest[Boolean implies true]: Some[true]

  test("inline (unknown implies false)"):
    InlinableTest[Boolean implies false]: None.type

  test("inline (true implies unknown)"):
    InlinableTest[true implies Boolean]: None.type

  test("inline (false implies unknown)"):
    InlinableTest[false implies Boolean]: Some[true]

  test("inline (unknown implies unknown)"):
    InlinableTest[Boolean implies Boolean]: None.type


  test("compute (false nand false)"):
    ComputeTest[false nand false](true)

  test("inline (true nand true)"):
    InlinableTest[true nand true]: Some[false]

  test("inline (true nand false)"):
    InlinableTest[true nand false]: Some[true]

  test("inline (false nand true)"):
    InlinableTest[false nand true]: Some[true]

  test("inline (false nand false)"):
    InlinableTest[false nand false]: Some[true]

  test("inline (unknown nand true)"):
    InlinableTest[Boolean nand true]: None.type

  test("inline (unknown nand false)"):
    InlinableTest[Boolean nand false]: Some[true]

  test("inline (true nand unknown)"):
    InlinableTest[true nand Boolean]: None.type

  test("inline (false nand unknown)"):
    InlinableTest[false nand Boolean]: Some[true]

  test("inline (unknown nand unknown)"):
    InlinableTest[Boolean nand Boolean]: None.type


  test("inline (true nor true)"):
    InlinableTest[true nor true]: Some[false]

  test("inline (true nor false)"):
    InlinableTest[true nor false]: Some[false]

  test("inline (false nor true)"):
    InlinableTest[false nor true]: Some[false]

  test("inline (false nor false)"):
    InlinableTest[false nor false]: Some[true]

  test("inline (unknown nor true)"):
    InlinableTest[Boolean nor true]: Some[false]

  test("inline (unknown nor false)"):
    InlinableTest[Boolean nor false]: None.type

  test("inline (true nor unknown)"):
    InlinableTest[true nor Boolean]: Some[false]

  test("inline (false nor unknown)"):
    InlinableTest[false nor Boolean]: None.type

  test("inline (unknown nor unknown)"):
    InlinableTest[Boolean nor Boolean]: None.type


  test("inline Not[true]"):
    InlinableTest[Not[true]]: Some[false]

  test("inline Not[false]"):
    InlinableTest[Not[false]]: Some[true]

  test("inline Not[unknown]"):
    InlinableTest[Not[Boolean]]: None.type


  test("inline (true or true)"):
    InlinableTest[true or true]: Some[true]

  test("inline (true or false)"):
    InlinableTest[true or false]: Some[true]

  test("inline (false or true)"):
    InlinableTest[false or true]: Some[true]

  test("inline (false or false)"):
    InlinableTest[false or false]: Some[false]

  test("inline (unknown or true)"):
    InlinableTest[Boolean or true]: Some[true]

  test("inline (unknown or false)"):
    InlinableTest[Boolean or false]: None.type

  test("inline (true or unknown)"):
    InlinableTest[true or Boolean]: Some[true]

  test("inline (false or unknown)"):
    InlinableTest[false or Boolean]: None.type

  test("inline (unknown or unknown)"):
    InlinableTest[Boolean or Boolean]: None.type


  test("inline (true xnor true)"):
    InlinableTest[true xnor true]: Some[true]

  test("inline (true xnor false)"):
    InlinableTest[true xnor false]: Some[false]

  test("inline (false xnor true)"):
    InlinableTest[false xnor true]: Some[false]

  test("inline (false xnor false)"):
    InlinableTest[false xnor false]: Some[true]

  test("inline (unknown xnor true)"):
    InlinableTest[Boolean xnor true]: None.type

  test("inline (unknown xnor false)"):
    InlinableTest[Boolean xnor false]: None.type

  test("inline (true xnor unknown)"):
    InlinableTest[true xnor Boolean]: None.type

  test("inline (false xnor unknown)"):
    InlinableTest[false xnor Boolean]: None.type

  test("inline (unknown xnor unknown)"):
    InlinableTest[Boolean xnor Boolean]: None.type


  test("inline (true xor true)"):
    InlinableTest[true xor true]: Some[false]

  test("inline (true xor false)"):
    InlinableTest[true xor false]: Some[true]

  test("inline (false xor true)"):
    InlinableTest[false xor true]: Some[true]

  test("inline (false xor false)"):
    InlinableTest[false xor false]: Some[false]

  test("inline (unknown xor true)"):
    InlinableTest[Boolean xor true]: None.type

  test("inline (unknown xor false)"):
    InlinableTest[Boolean xor false]: None.type

  test("inline (true xor unknown)"):
    InlinableTest[true xor Boolean]: None.type

  test("inline (false xor unknown)"):
    InlinableTest[false xor Boolean]: None.type

  test("inline (unknown xor unknown)"):
    InlinableTest[Boolean xor Boolean]: None.type


//
//  test("Some"):
//    InlinableTest[Some[Some[1]]]: Some[Some[Some[1]]]