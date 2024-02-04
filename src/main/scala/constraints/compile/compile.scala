package constraints.compile

import constraints.Guarantee

extension (guarantee: Guarantee.type)

  /**
   * Checks a constraint at compile time, failing to compile if the constraint cannot be confirmed to hold
   *
   * @tparam C the constraint
   * @return evidence that the constraint holds if the compile time check succeeds
   */
  inline def verifyAtCompileTime[C: Inlinable.To[Boolean]]: Guarantee[C] =
    inline Inlinable.reduce[C] match
      case Some(false) => compiletime.error("invalid")
      case None => compiletime.error("unknown")
      case Some(true) => Guarantee.trust[C]

  inline def apply[C: Inlinable.To[Boolean]]: Guarantee[C] = verifyAtCompileTime[C]

inline given [C: Inlinable.To[Boolean]]: Guarantee[C] = Guarantee[C]
