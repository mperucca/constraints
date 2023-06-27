package constraints

import org.scalatest.funsuite.AnyFunSuite

class IterateTest extends AnyFunSuite:

  test("Iterable"):
    val iterate = summon[Iterate[Iterable[Int], Int]]
    val iterable = Iterable(1, 2, 3)
    assert(iterate.toIterable(iterable) == iterable)

  test("String"):
    val iterate = summon[Iterate[String, Int]]
    val string = "abc"
    assert(iterate.toIterable(string).mkString == string.codePoints().nn.toArray.nn.mkString)

  test("Tuple"):
    val iterate = summon[Iterate[(Int, String, Boolean), Int | String | Boolean]]
    val tuple = (1, "", true)
    assert(iterate.toIterable(tuple).toList == tuple.toList)
