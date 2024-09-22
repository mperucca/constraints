Guarantees
===============

Underspecified APIs leave pitfalls for API users.
The goal of this library is to enable fully-specified APIs with ease.

The main idea is to specify constraints through `Guarantee`s. Suppose we want to provide an integer division function that enforces a non-zero divisor as well as no overflow. We can model that as follows:
```scala 3
def divide(a: Int, b: Int)(using
  noDivideByZero: Guarantee[b.type != 0],
  noOverflow: Guarantee[a.type != Int.MinValue.type or b.type != -1]
): Int = a / b
```
Attempts to call <code>divide</code> must provide `Guarantee`s which can be obtained through various ways:
* `trust` which just constructs one without checking
* <code>test</code> which checks at runtime through `Compute.To[Boolean]` type class instances
* `apply` which attempts to verify the constraint at compile time (still under development)

Here's how a caller could call `divide` safely when working with runtime values (presumably the most common scenario):
```scala 3
val a, b = Random.nextInt()

Guarantee.test[b.type != 0 and (a.type != Int.MinValue.type or b.type != -1)] match
  case Right(given Guarantee[b.type != 0 and (a.type != Int.MinValue.type or b.type != -1)]) =>
    divide(a, b) // compiles because the necessary Guarantee is in scope
  case Left(given Guarantee[b.type == 0 or (a.type == Int.MinValue.type and b.type == -1)]) =>
    // divide(a, b) would be a compile error since the Guarantee is invalid
```
`Guarantee.test[Constraint]` returns `Either[Guarantee[Not[Constraint]], Guarantee[Constraint]]` following the convention of the `Right` side being successful.
At runtime, the constraint check will run, and only if the `Guarantee` is acquired through the `Right` case are we able to call `divide` without a compile error. Notice how we only have one `given` `Guarantee` in scope for the `Right` `case`, but it is sufficient for both `Guarantee` parameters of `divide`.
This is due to `and` being translated into the intersection type `&`, and it is found during implicit search for both `using` `Guarantee` parameters of `divide`.

Looking at the `Left` `case`, you may be asking, "Where is the `Not` in the above example? I thought you said `Guarantee.test` is supposed to return the negated `Guarantee`."
Well, one neat thing about `Guarantee` is that it knows DeMorgan's laws and represents Boolean predicates in their simplest forms. This means the type system can tell that the following examples type check:
```scala 3
summon[
  Guarantee[b.type == 0 or (a.type == Int.MinValue.type and b.type == -1)]
    =:=
    Guarantee[Not[b.type != 0 and (a.type != Int.MinValue.type or b.type != -1)]]
]

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
```
The types `==`, `!=`, and other common functions, as well as the combinators `and`, `or`, `Not`, and others as well are provided by the library.

Let's look at another example: how to safely merge two sorted lists.

There are many ways to sort a list, so merging two lists requires the lists be sorted the same way. Here's an unsafe merge than relies on, but doesn't enforce, this sorted condition to merge sorted lists in linear time.

```scala 3
def mergeUnsafe[A](list1: List[A], list2: List[A])(using comp: Ordering[A]): List[A] =
  (list1, list2) match
    case (Nil, l2) => l2
    case (l1, Nil) => l1
    case (l1 @ h1 :: t1, l2 @ h2 :: t2) =>
      if comp.lt(h1, h2)
      then h1 :: mergeByUnsafe(t1, l2)
      else h2 :: mergeByUnsafe(l1, t2)
```

Here's how a safer alternative might be represented:

```scala 3
def merge[A](list1: List[A], list2: List[A])(using comp: Ordering[A])(
  using Guarantee[Sorted[comp.type, list1.type]], Guarantee[Sorted[comp.type, list2.type]]
): SortedList[A, comp.type] =
  Guaranteed.Refined(mergeUnsafe(list1, list2))(trust)

// Represents the constraint that the comparator C has sorted L
type Sorted[C, L]

// A SortedList is a List, but it also carries a Guarantee that it's sorted
type SortedList[A, C] = Guaranteed.Refined[List[A], [L] =>> Sorted[C, L]]

// Sorts a list with the standard library, but it also attaches the sorted guarantee
def sort[A](list: List[A])(using comp: Ordering[A]): SortedList[A, comp.type] =
  Guaranteed.Refined(list.sorted)(Guarantee.trust)
```
We can use this function as follows:
```scala 3
// sort the lists
val list1, list2 = sort(List.fill(3)(Random.nextInt(9)))
// bring the sorted list guarantees into scope
import list1.guarantee
import list2.guarantee
// merge the sorted lists together
merge(list1.value, list2.value)
```
Using `List[Int]`s means the implicit `Ordering.Int` instance will be used. Sorting one of the lists instead by `Ordering.Int.reverse` will produce a compile error.