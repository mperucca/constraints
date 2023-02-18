package constraints

/**
 * Evidence that constraint [[A]] is met
 */
type Trust[A] = Trust.Impl[Normalize[A]]

/**
 * Factory methods for [[Trust]] instances
 */
object Trust:

  /**
   * The evidence type for a constraint
   * @tparam A the constraint (covariant so that more specific constraints satisfy more general constraints)
   */
  private[constraints] sealed trait Impl[+A]

  /**
   * The evidence implementation (constraint type is [[Nothing]] so that it satisfies all constraints)
   */
  private[constraints] object Impl extends Impl[Nothing]

  /**
   * Returns an evidence based on blind trust
   * @tparam A the constraint to trust
   * @return the trusted evidence
   */
  def belief[A]: Trust[A] = Impl

  /**
   * Checks a constraint at runtime, returning evidence or negative evidence of the constraint
   * @param runtimeCheck the runtime check to perform
   * @tparam A the constraint
   * @return evidence that the constraint either holds or does not
   */
  def runtimeCheck[A](using runtimeCheck: RuntimeCheck[A]): Either[Trust[not[A]], Trust[A]] =
    Either.cond(runtimeCheck.succeeded, belief, belief)

  /**
   * Checks a constraint at compile time, failing to compile if the constraint cannot be confirmed to hold
   * @param compileTimeCheck the compile time check to perform
   * @tparam A the constraint
   * @return evidence that the constraint holds if the compile time check succeeds
   */
  inline def compileTimeCheck[A](using inline compileTimeCheck: CompileTimeCheck[A]): Trust[A] =
    inline compileTimeCheck.valid match
      case false => compiletime.error("invalid")
      case null => compiletime.error("unknown")
      case true => belief

  extension [A](trust: => Trust[A])

    /**
     * Joins two constraint evidences into one
     */
    infix def and[B](other: => Trust[B]): Trust[A and B] = belief
