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
   * Returns a guarantee based on blind trust
   * 
   * @tparam C the constraint to trust
   * @return the trusted guarantee
   */
  def trust[C]: Guarantee[C] = Guarantee

  /**
   * Checks a constraint at runtime, returning a guarantee for or against the constraint
   * 
   * @param predicate the runtime check to perform
   * @tparam C the constraint
   * @return either a guarantee that the constraint holds or a guarantee that it does not
   */
  def testAtRuntime[C](using predicate: Computable.Predicate[C]): Either[Guarantee[Not[C]], Guarantee[C]] =
    Either.cond(predicate.compute, trust, trust)

  /**
   * Checks a constraint at compile time, failing to compile if the constraint cannot be confirmed to hold
   * 
   * @param predicate the compile time check to perform
   * @tparam C the constraint
   * @return evidence that the constraint holds if the compile time check succeeds
   */
  inline given verifyAtCompileTime[C](using predicate: Inlinable.Predicate[C]): Guarantee[C] =
    inline predicate.reduce match
      case Some(false) => compiletime.error("invalid")
      case None => compiletime.error("unknown")
      case Some(true) => trust

  extension [A](guarantee: => Guarantee[A])

    /**
     * Joins two constraint evidences into one
     */
    infix def and[B](other: => Guarantee[B]): Guarantee[A and B] = trust
