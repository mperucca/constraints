package constraint

import quoted.*

trait CompileTimeCheck[-A]:
  inline def satisfied: Boolean | Null

object CompileTimeCheck:

  transparent inline given[A, B](
    using inline a: CompileTimeCheck[A], inline b: CompileTimeCheck[B]
  ): CompileTimeCheck[A And B] =
    inline a.satisfied match
      case false => FalseCompileTimeCheck
      case null => inline b.satisfied match
        case false => FalseCompileTimeCheck
        case null | true => UnknownCompileTimeCheck
      case true => inline b.satisfied match
        case false => FalseCompileTimeCheck
        case null => UnknownCompileTimeCheck
        case true => TrueCompileTimeCheck

  transparent inline given[A](
    using inline a: CompileTimeCheck[A]
  ): CompileTimeCheck[Not[A]] =
    inline a.satisfied match
      case false => TrueCompileTimeCheck
      case null => UnknownCompileTimeCheck
      case true => FalseCompileTimeCheck

  transparent inline given[A, B](
    using inline a: CompileTimeCheck[A], inline b: CompileTimeCheck[B]
  ): CompileTimeCheck[A Or B] =
    inline a.satisfied match
      case false => inline b.satisfied match
        case false => FalseCompileTimeCheck
        case null => UnknownCompileTimeCheck
        case true => TrueCompileTimeCheck
      case null => inline b.satisfied match
        case false | null => UnknownCompileTimeCheck
        case true => TrueCompileTimeCheck
      case true => TrueCompileTimeCheck

  transparent inline given[A, B](
    using inline a: CompileTimeCheck[A], inline b: CompileTimeCheck[B]
  ): CompileTimeCheck[A Xor B] =
    inline a.satisfied match
      case false => inline b.satisfied match
        case false => FalseCompileTimeCheck
        case null => UnknownCompileTimeCheck
        case true => TrueCompileTimeCheck
      case null => UnknownCompileTimeCheck
      case true => inline b.satisfied match
        case false => TrueCompileTimeCheck
        case null => UnknownCompileTimeCheck
        case true => FalseCompileTimeCheck

  transparent inline given CompileTimeCheck[True] = TrueCompileTimeCheck
  transparent inline given CompileTimeCheck[False] = FalseCompileTimeCheck
  transparent inline given CompileTimeCheck[Null] = UnknownCompileTimeCheck

  private object FalseCompileTimeCheck extends CompileTimeCheck[Any]:
    override inline def satisfied: false = false

  private object UnknownCompileTimeCheck extends CompileTimeCheck[Any]:
    override inline def satisfied: Null = null

  private object TrueCompileTimeCheck extends CompileTimeCheck[Any]:
    override inline def satisfied: true = true
