import constraints.*

import quoted.*
import scala.annotation.targetName

type Equal[A, B]
@targetName("Equal")
type ===[A, B] = Equal[A, B]

object Equal:

  given computable[A, B](using a: Computable.Typed[A, Any], b: Computable.Typed[B, Any]): Computable.Predicate[A === B] =
    Computable(a.compute == b.compute)

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
    Inlinable.fromComputablePostponingExtractableCheck[(A, B), Boolean]: (a, b) =>
      computable[a.type, b.type]
