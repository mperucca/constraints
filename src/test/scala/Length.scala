import constraints.*
import constraints.Computation.Typed

import scala.quoted.{Expr, Quotes, Type}

type Length[I]

object Length:

  given computation[S](
    using c: Computation.Typed[S, String]
  ): Computation.Typed[Length[S], Int] =
    Computation(c.compute.length)

  transparent inline given inlinable[S](
    using c: Inlinable.Typed[S, String]
  ): Inlinable.Typed[Length[S], Int] =
    inline c.reduce match
      case null => Inlinable.Unknown
      case s: String => Impl[s.type]

  class Impl[S <: String] extends Inlinable.Impl[Int]:
    override transparent inline def reduce: Int | Null = ${ impl[S] }

  private def impl[S <: String : Type](using Quotes): Expr[Int | Null] =
    Inlinable.fromComputation((s: S) => computation[s.type])