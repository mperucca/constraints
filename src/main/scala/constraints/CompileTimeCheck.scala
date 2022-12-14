package constraints

trait CompileTimeCheck[-A]:
  inline def valid: false | Null | true

object CompileTimeCheck:

  def fromRuntimeCheckOnPossibleConstant[A: quoted.Type](runtimeCheck: (a: A) => RuntimeCheck[Nothing])(using quoted.Quotes): quoted.Expr[false | Null | true] =
    quoted.Type.valueOfConstant[A].fold('{null})(a => fromRuntimeCheck(using runtimeCheck(a)))

  def fromRuntimeCheckOnPossibleConstantTuple[T <: Tuple: quoted.Type](runtimeCheck: (t: T) => RuntimeCheck[Nothing])(using quoted.Quotes): quoted.Expr[false | Null | true] =
    quoted.Type.valueOfTuple[T].fold('{ null })(t => fromRuntimeCheck(using runtimeCheck(t)))

  def fromRuntimeCheck[A](using runtimeCheck: RuntimeCheck[A])(using quoted.Quotes): quoted.Expr[true | false] =
    if runtimeCheck.succeeded then '{true} else '{false}

  private object False extends CompileTimeCheck[Any]:
    override inline def valid: false = false

  private object Unknown extends CompileTimeCheck[Any]:
    override inline def valid: Null = null

  private object True extends CompileTimeCheck[Any]:
    override inline def valid: true = true

  transparent inline given falsehood: CompileTimeCheck[false] = False
  transparent inline given unknown: CompileTimeCheck[Null] = Unknown
  transparent inline given truth: CompileTimeCheck[true] = True

  transparent inline given[A](using inline a: CompileTimeCheck[A]): CompileTimeCheck[not[A]] =
    inline a.valid match
      case false => True
      case null => Unknown
      case true => False

  transparent inline given [A, B](
    using inline a: CompileTimeCheck[A], inline b: CompileTimeCheck[B]
  ): CompileTimeCheck[A and B] =
    inline a.valid match
      case false => False
      case null => inline b.valid match
        case false => False
        case null | true => Unknown
      case true => inline b.valid match
        case false => False
        case null => Unknown
        case true => True

  transparent inline given [A, B](
    using inline a: CompileTimeCheck[A], inline b: CompileTimeCheck[B]
  ): CompileTimeCheck[A or B] =
    inline a.valid match
      case false => inline b.valid match
        case false => False
        case null => Unknown
        case true => True
      case null => inline b.valid match
        case false | null => Unknown
        case true => True
      case true => True

  transparent inline given [A, B](
    using inline a: CompileTimeCheck[A], inline b: CompileTimeCheck[B]
  ): CompileTimeCheck[A xor B] =
    inline a.valid match
      case false => inline b.valid match
        case false => False
        case null => Unknown
        case true => True
      case null => Unknown
      case true => inline b.valid match
        case false => True
        case null => Unknown
        case true => False
