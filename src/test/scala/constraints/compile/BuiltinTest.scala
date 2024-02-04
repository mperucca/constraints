package constraints.compile

import constraints.compile.*
import org.scalatest.funsuite.AnyFunSuite

import scala.util.NotGiven

class BuiltinTest extends AnyFunSuite:

  test("primitives"):
    summon[Builtin[String]]
    summon[Builtin[Boolean]]
    summon[Builtin[Byte]]
    summon[Builtin[Char]]
    summon[Builtin[Double]]
    summon[Builtin[Float]]
    summon[Builtin[Int]]
    summon[Builtin[Long]]
    summon[Builtin[Short]]

  test("tuple"):
    type A
    summon[Builtin[EmptyTuple]]
    summon[Builtin[Primitive *: EmptyTuple]]
    summon[Builtin[(Primitive, Primitive)]]

  test("custom"):
    type A
    summon[NotGiven[Builtin[A]]]
    summon[NotGiven[A *: EmptyTuple]]

  test("subtypes"):
    type A <: Primitive
    summon[Builtin[A]]
    summon[Builtin[1]]
    summon[Builtin[(2, 3)]]
    val tuple: 4 *: EmptyTuple = null.asInstanceOf
    summon[Builtin[tuple.type]]
