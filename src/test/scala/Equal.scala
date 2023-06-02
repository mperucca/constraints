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

  transparent inline given inliner[A, B](
    using a: Inliner.Typed[A, Any], b: Inliner.Typed[B, Any]
  ): Inliner.Predicate[A === B] =
    inline a.reduce match
      case null => Inliner.Unknown
      case r1: Primitive => continue[r1.type, B] // use literal type for primitive
      case r1: Tuple => continue[a.Result, B] // literal types don't reduce for tuples

  transparent inline def continue[A, B](
    using b: Inliner.Typed[B, Any]
  ): Inliner.Predicate[Any] =
    inline b.reduce match
      case null => Inliner.Unknown
      case r2: Primitive => InlinerImpl[A, r2.type]
      case r2: Tuple => InlinerImpl[A, b.Result]

  class InlinerImpl[A, B] extends Inliner.Impl[Boolean]:
    override transparent inline def reduce: Boolean | Null = ${ impl[A, B] }

  private def impl[A : Type, B : Type](using Quotes): Expr[Boolean | Null] =
    Inliner.fromComputationPostponingExtractableCheck[(A, B), Boolean] { case (a, b) =>
      computation[a.type, b.type]
    }
