Guarantees
===============

Underspecified APIs leave pitfalls for API users.
The goal of this library is to enable fully-specified APIs with ease.

The main idea is to specify constraints through `Guarantee`s as follows:
```scala
def divide(a: Int, b: Int)(using
  Guarantee[b.type != 0],
  Guarantee[a.type != Int.MinValue.type or b.type != -1]
): Int = a / b
```
Attempts to call <code>divide</code> must provide `Guarantee`s which can be obtained through various ways:
* `trust` which just constructs one without checking
* <code>test</code> which checks at runtime through `Compute.To[Boolean]` type class instances
* `apply` which attempts to verify the constraint at compile time

Here's how a caller could call `divide` safely when working with runtime values (presumably the most common scenario):
```scala
val a, b = Random.nextInt()

Guarantee.test[b.type != 0 and (a.type != Int.MinValue.type or b.type != -1)] match
  case Right(given Guarantee[b.type != 0 and (a.type != Int.MinValue.type or b.type != -1)]) =>
    divide(a, b) // compiles because the necessary Guarantee is in scope
  case Left(given Guarantee[b.type == 0 or (a.type == Int.MinValue.type and b.type == -1)]) =>
    // divide(a, b) would be a compile error since the Guarantee is invalid
```
`Guarantee.test[Constraint]` returns `Either[Guarantee[Not[Constraint]], Guarantee[Constraint]]` following the convention of the `Right` side being successful.
Notice how we only have one `Guarantee` in scope for the `Right` `case`, but it is sufficient for both `Guarantee` parameters of `divide`.
This is due to `and` being translated into the intersection type `&`.

You may also be asking though, "Where is the `Not` in the above example for the `Left` `case`?"
One neat thing about `Guarantee` is that it knows DeMorgan's laws and represents Boolean predicates in their simplest forms.

This all means the type system can tell that the following type checks:
```scala
summon[
  Guarantee[b.type != 0 and (a.type != Int.MinValue.type or b.type != -1)]
    <:<
  Guarantee[b.type != 0]
]

summon[
  Guarantee[b.type != 0 and (a.type != Int.MinValue.type or b.type != -1)]
    <:<
  Guarantee[a.type != Int.MinValue.type or b.type != -1]
]

summon[
  Guarantee[b.type == 0 or (a.type == Int.MinValue.type and b.type == -1)]
    =:=
  Guarantee[Not[b.type != 0 and (a.type != Int.MinValue.type or b.type != -1)]]
]
```
More documentation to come!
