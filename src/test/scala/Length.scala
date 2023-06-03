import constraints.*
import constraints.Computable.Typed

import scala.quoted.{Expr, Quotes, Type}

type Length[I]

object Length:

  given computable[S](using c: Computable.Typed[S, String]): Computable.Typed[Length[S], Int] =
    Computable(c.compute.length)

  transparent inline given inlinable[S](
    using c: Inlinable.Typed[S, String]
  ): Inlinable.Typed[Length[S], Int] =
    inline c.reduce match
      case None => Inlinable.Unknown
      case Some(s: String) => Impl[s.type]

  class Impl[S <: String] extends Inlinable.Impl[Int]:
    override transparent inline def reduce: Option[Int] = ${ impl[S] }

  private def impl[S <: String : Type](using Quotes): Expr[Option[Int]] =
    Inlinable.fromComputable((s: S) => computable[s.type])