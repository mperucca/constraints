import constraints.*

import quoted.*
import scala.annotation.targetName

type Equal[A, B]
@targetName("Equal")
type ===[A, B] = Equal[A, B]

object Equal:

  given computable[A: Compute, B: Compute]: Compute.Typed[A === B, Boolean] =
    Compute(Compute[A] == Compute[B])

  transparent inline given inlinable[A, B: Inlinable](
    using a: Inlinable.Typed[A, Any]
  ): Inlinable.Typed[A === B, Boolean] =
    inline a.reduce match
      case None => Inlinable.Unknown
      case Some(r1: Primitive) => continue[r1.type, B] // use literal type for primitive
      case Some(r1: Tuple) => continue[a.Result, B] // literal types don't reduce for tuples

  transparent inline def continue[A, B](
    using b: Inlinable.Typed[B, Any]
  ): Inlinable.Typed[Any, Boolean] =
    inline b.reduce match
      case None => Inlinable.Unknown
      case Some(r2: Primitive) => Impl[A, r2.type]
      case Some(r2: Tuple) => Impl[A, b.Result]

  class Impl[A, B] extends Inlinable.Impl[Boolean]:
    override transparent inline def reduce: Option[Boolean] = ${ impl[A, B] }

  private def impl[A : Type, B : Type](using Quotes): Expr[Option[Boolean]] =
    Inlinable.fromComputationPostponingExtractableCheck[(A, B), Boolean]: (a, b) =>
      computable[a.type, b.type]
