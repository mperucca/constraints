package constraints

trait RuntimeComputation[-E]:
  type Result
  def result: Result

object RuntimeComputation:

  type Typed[-E, +R] = RuntimeComputation[E] { type Result <: R }

  type Predicate[-E] = Typed[E, Boolean]

  def apply[E, R](result: => R): RuntimeComputation.Typed[E, R] =
    lazy val _result = result
    new RuntimeComputation[E] { type Result = R; lazy val result: R = _result }

  given literalSingleton[R <: Singleton: ValueOf]: RuntimeComputation.Typed[R, R] =
    literal

  given literal[R: ValueOf]: RuntimeComputation.Typed[R, R] =
    RuntimeComputation(valueOf[R])

  /**
   * The type class instance for the negation of constraints: [[Not]]
   *
   * @param c the runtime check to invert
   * @tparam C the constraint
   * @return a runtime check that succeeds if the runtime check for [[C]] fails
   */
  given[C](using c: RuntimeComputation.Predicate[C]): RuntimeComputation.Predicate[Not[C]] =
    RuntimeComputation(!c.result)

  /**
   * The type class instance for the conjunction of constraints: [[and]]
   *
   * @param a runtime check for the first constraint
   * @param b runtime check for the second constraint (by-name as it's unnecessary if the first runtime check fails)
   * @tparam A the first constraint
   * @tparam B the second constraint
   * @return a runtime check that succeeds if both runtime checks succeed
   */
  given[A, B](
    using a: RuntimeComputation.Predicate[A], b: RuntimeComputation.Predicate[B]
  ): RuntimeComputation.Predicate[A and B] =
    RuntimeComputation(a.result && b.result)

  /**
   * The type class instance for the inclusive disjunction of constraints [[or]]
   *
   * @param a runtime check for the first constraint
   * @param b runtime check for the second constraint (by-name as it's unnecessary if the first runtime check succeeds)
   * @tparam A the first constraint
   * @tparam B the second constraint
   * @return a runtime check that succeeds if either runtime check succeeds
   */
  given[A, B](
    using a: RuntimeComputation.Predicate[A], b: RuntimeComputation.Predicate[B]
  ): RuntimeComputation.Predicate[A or B] =
    RuntimeComputation(a.result || b.result)

  /**
   * The type class instance for the exclusive disjunction of constraints [[xor]]
   *
   * @param a a runtime check for the first constraint
   * @param b a runtime check for the second constraint
   * @tparam A the first constraint
   * @tparam B the second constraint
   * @return the runtime check that succeeds if one but not both of the runtime checks succeeds
   */
  given[A, B](
    using a: RuntimeComputation.Predicate[A], b: RuntimeComputation.Predicate[B]
  ): RuntimeComputation.Predicate[A xor B] =
    RuntimeComputation(a.result != b.result)

