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

  given truth: Guarantee[true] = trust[true]

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
   * @return either a guarantee that the constraint holds ([[Right]]) or a guarantee that it does not ([[Left]])
   */
  def test[C: Compute.To[Boolean]]: Tested[C] =
    Either.cond(Compute[C], trust, trust)

  type Tested[C] = Either[Guarantee[Not[C]], Guarantee[C]]

  extension [C](tested: Tested[C])
    def ifElse[A](ifNotGuarantee: Guarantee[C] ?=> A)(ifGuarantee: Guarantee[Not[C]] ?=> A): A =
      tested.fold(ifNotGuarantee(using _), ifGuarantee(using _))
    def ifGuaranteed[A](ifGuarantee: Guarantee[C] ?=> A): Either[Guarantee[Not[C]], A] =
      tested.map(ifGuarantee(using _))
    def ifNotGuaranteed[A](ifNotGuarantee: Guarantee[Not[C]] ?=> A): Either[A, Guarantee[C]] =
      tested.left.map(ifNotGuarantee(using _))
    def orElse(ifNotGuarantee: Guarantee[Not[C]] ?=> Guarantee[C]): Guarantee[C] =
      tested.fold(ifNotGuarantee(using _), identity)

  /**
   * Starts a chain of accumulating failed guarantee tests into a single type.
   * @tparam E the accumulated type for guarantee tests that fail
   * @return A function expecting a constraint to begin the guarantee chain of tests
   */
  def orAccumulate[E]: Accumulated[true, E] = Accumulated(Right(Guarantee.truth))

  /**
   * Helper class for accumulating guarantee test chains
   * @param accumulated The already accumulated guarantee tests
   * @tparam A The accumulated constraint so far
   * @tparam E The accumulated failed test type
   */
  class Accumulated[A, E](accumulated: Either[List[E], Guarantee[A]]):

    /**
     * Chains on another constraint to check
     * @param ifNot the function the call if the constraint test fails
     * @tparam C the new constraint to check
     * @return the newly accumulated constraint test
     */
    def test[C: Compute.To[Boolean]](ifNot: Guarantee[Not[C]] => E): Accumulated[A and C, E] =
      Accumulated(
        Guarantee.test[C] match
          case Left(not) => Left(::(ifNot(not), result.left.getOrElse(Nil)))
          case Right(guarantee) => result.map(guarantee and _)
      )

    /**
     * @see [[test]] except that the negated guarantee argument is ignored
     */
    def test_[C: Compute.To[Boolean]](ifNot: => E): Accumulated[A and C, E] =
      test(_ => ifNot)

    /**
     * @see [[test]] except that the negated guarantee argument is contextual
     */
    def test$[C: Compute.To[Boolean]](ifNot: Guarantee[Not[C]] ?=> E): Accumulated[A and C, E] =
      test(ifNot(using _))

    /**
     * @return The accumulated guarantee or failed test results in the case of failures
     */
    def result: Either[::[E], Guarantee[A]] = accumulated.left.map(_.reverse.asInstanceOf[::[E]])

  extension [A](guarantee: => Guarantee[A])

    /**
     * Joins two constraint evidences into one
     */
    infix def and[B](other: => Guarantee[B]): Guarantee[A and B] = trust
