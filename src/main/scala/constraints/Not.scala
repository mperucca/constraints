package constraints

/**
 * Represents negation (sometimes represented by the symbol ¬)
 *
 * @tparam A the term to negate
 */
sealed trait Not[A]

object Not:

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
   * Type class instance of [[CompileTimeComputation]] for [[Not]]
   *
   * @param c the [[CompileTimeComputation]] instance to negate
   * @tparam C the constraint to negate
   * @return a [[CompileTimeComputation]] for the negation of [[C]]
   *         [[Not]] on false becomes true
   *         [[Not]] on unknown stays unknown
   *         [[Not]] on true becomes false
   */
  transparent inline given[C](
    using inline c: CompileTimeComputation.Predicate[C]
  ): CompileTimeComputation.Predicate[Not[C]] =
    inline c.result match
      case false => CompileTimeComputation.Constant[true]
      case null => CompileTimeComputation.Unknown
      case true => CompileTimeComputation.Constant[false]