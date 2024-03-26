Guarantees
===============

Underspecified APIs leave pitfalls for API users.
The goal of this library is to enable fully-specified APIs with ease.

The main idea is to specify constraints through `Guarantee`s as follows:
```scala
def divide(dividend: Int, divisor: Int)(using
  Guarantee[divisor.type !== 0],
  Guarantee[dividend.type !== Int.MinValue.type or divisor.type !== -1]
): Int = dividend / divisor
```
Attempts to call <code>divide</code> must provide `Guarantee`s which can be obtained through various ways:
* `trust` which just constructs one without checking
* <code>test</code> which checks at runtime through `Compute.To[Boolean]` type class instances
* `apply` which attempts to verify the constraint at compile time

Here's how a caller could call `divide` safely when working with runtime values (presumably the most common scenario):
```scala
val dividend, divisor = Random.nextInt()

Guarantee.test[divisor.type !== 0 and (dividend.type !== Int.MinValue.type or divisor.type !== -1)] match
  case Right(given Guarantee[divisor.type !== 0 and (dividend.type !== Int.MinValue.type or divisor.type !== -1)]) =>
    divide(dividend, divisor) // compiles because the necessary Guarantee is in scope
  case Left(given Guarantee[divisor.type === 0 or (dividend.type === Int.MinValue.type and divisor.type === -1)]) =>
    // divide(dividend, divisor) would be a compile error since the Guarantee is invalid
```
`Guarantee.test[Constraint]` returns `Either[Guarantee[Not[Constraint]], Guarantee[Constraint]]` following the convention of the `Right` side being successful.
Notice how we only have one `Guarantee` in scope for the `Right` `case`, but it is sufficient for both `Guarantee` parameters of `divide`.
This is due to `and` being translated into the intersection type `&`.

You may also be asking though, "Where is the `Not` in the above example for the `Left` `case`?"
One neat thing about `Guarantee` is that it knows DeMorgan's laws and represents Boolean predicates in their simplest forms.

This all means the type system can tell that the following type checks:
```scala
summon[
  Guarantee[divisor.type !== 0 and (dividend.type !== Int.MinValue.type or divisor.type !== -1)]
    <:<
  Guarantee[divisor.type !== 0]
]

summon[
  Guarantee[divisor.type !== 0 and (dividend.type !== Int.MinValue.type or divisor.type !== -1)]
    <:<
  Guarantee[dividend.type !== Int.MinValue.type or divisor.type !== -1]
]

summon[
  Guarantee[divisor.type === 0 or (dividend.type === Int.MinValue.type and divisor.type === -1)]
    =:=
  Guarantee[Not[divisor.type !== 0 and (dividend.type !== Int.MinValue.type or divisor.type !== -1)]]
]
```
More documentation to come!
