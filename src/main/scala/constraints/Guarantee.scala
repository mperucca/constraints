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

  extension [A](guarantee: => Guarantee[A])

    /**
     * Joins two constraint evidences into one
     */
    infix def and[B](other: => Guarantee[B]): Guarantee[A and B] = trust
