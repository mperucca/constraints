package constraints

/**
 * Evidence that constraint [[C]] is met
 */
type Guarantee[C] = Guarantee.Impl[Normalize[C]]

/**
 * Factory methods for [[Guarantee]] instances
 */
object Guarantee:

  /**
   * The (hidden) evidence type for a constraint
   *
   * @tparam C the constraint (covariant so that more specific constraints satisfy more general constraints)
   */
  private[constraints] opaque type Impl[+C] = Guarantee.type

  /**
   * Add {{{import Guarantee.Everything.given}}} to supply trust to all {{{using Guarantee}}} values in scope.
   */
  object Everything:
    given Impl[Nothing] = Guarantee

  /**
   * Returns a guarantee based on blind trust
   *
   * @tparam C the constraint to trust
   * @return the trusted guarantee
   */
  def trust[C]: Guarantee[C] = Guarantee

  /**
   * Checks a constraint at runtime, returning a guarantee for or against the constraint
   *
   * @tparam C the constraint
   * @return either a guarantee that the constraint holds or a guarantee that it does not
   */
  def test[C: Compute.To[Boolean]]: Either[Guarantee[Not[C]], Guarantee[C]] =
    Either.cond(Compute[C], trust, trust)

  /**
   * Starts a chain of accumulating failed guarantee tests into a single type.
   * @tparam E the accumulated type for guarantee tests that fail
   * @return A function expecting a constraint to begin the guarantee chain of tests
   */
  def accumulate[E]: [C] => Compute.Typed[C, Boolean] ?=> (Guarantee[Not[C]] => E) => Accumulated[C, E] =
    [C] => compute ?=> ifNot => Accumulated(test[C].left.map(not => ::(ifNot(not), Nil)))

  /**
   * Helper class for accumulating guarantee test chains
   * @param accumulated The already accumulated guarantee tests
   * @tparam A The accumulated constraint so far
   * @tparam E The accumulated failed test type
   */
  class Accumulated[A, E](accumulated: Either[::[E], Guarantee[A]]):

    /**
     * Chains on another constraint to check
     * @param ifNot the function the call if the constraint test fails
     * @tparam C the new constraint to check
     * @return the newly accumulated constraint test
     */
    def apply[C: Compute.To[Boolean]](ifNot: Guarantee[Not[C]] => E): Accumulated[A and C, E] =
      Accumulated(
        test[C] match
          case Left(not) => Left(::(ifNot(not), result.left.getOrElse(Nil)))
          case Right(guarantee) => result.map(guarantee and _)
      )

    /**
     * @return The accumulated guarantee or failed test results in the case of failures
     */
    def result: Either[::[E], Guarantee[A]] = accumulated.left.map(_.reverse.asInstanceOf[::[E]])

  extension [A](guarantee: => Guarantee[A])

    /**
     * Joins two constraint evidences into one
     */
    infix def and[B](other: => Guarantee[B]): Guarantee[A and B] = trust
