import constraints.*

import quoted.*
import scala.annotation.targetName

type Equal[A, B]
@targetName("Equal")
type ===[A, B] = Equal[A, B]

object Equal:

  given computation[A, B](
    using a: Computation.Typed[A, Any], b: Computation.Typed[B, Any]
  ): Computation.Predicate[A === B] =
    Computation(a.compute == b.compute)

  transparent inline given inlinable[A, B](
    using a: Inlinable.Typed[A, Any], b: Inlinable.Typed[B, Any]
  ): Inlinable.Predicate[A === B] =
    inline a.reduce match
      case null => Inlinable.Unknown
      case r1: Primitive => continue[r1.type, B] // use literal type for primitive
      case r1: Tuple => continue[a.Result, B] // literal types don't reduce for tuples

  transparent inline def continue[A, B](
    using b: Inlinable.Typed[B, Any]
  ): Inlinable.Predicate[Any] =
    inline b.reduce match
      case null => Inlinable.Unknown
      case r2: Primitive => Impl[A, r2.type]
      case r2: Tuple => Impl[A, b.Result]

  class Impl[A, B] extends Inlinable.Impl[Boolean]:
    override transparent inline def reduce: Boolean | Null = ${ impl[A, B] }

  private def impl[A : Type, B : Type](using Quotes): Expr[Boolean | Null] =
    Inlinable.fromComputationPostponingExtractableCheck[(A, B), Boolean] { case (a, b) =>
      computation[a.type, b.type]
    }
