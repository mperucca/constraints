package constraints

import quoted.*

/**
 * A type class for constraint checking that is performed at compile time
 * 
 * @tparam C the constraint to check
 */
trait CompileTimeCheck[-C]:

  /**
   * The check to run at compile time.
   * If the inlining process resolves to a literal false, null, or true, then constraints can compose.
   * 
   * @return false if the constraint doesn't hold,
   *         null if constraint satisfaction cannot be determined, and
   *         true if the constraint holds
   */
  inline def valid: false | Null | true

/**
 * Utility methods and type class instances for provided combinators
 */
object CompileTimeCheck:

  /**
   * Utility method for [[CompileTimeCheck]] macro implementations that evaluates a constraint by attempting to
   * extract a constant value from type [[V]] and then calling the provided [[RuntimeCheck]] with the value
   * 
   * @param runtimeCheck a function returning a [[RuntimeCheck]] when given the value extracted from [[V]]
   * @param Quotes performs operations on expressions
   * @tparam V the type from which to attempt extracting a value
   * @return an expression that is
   *         false if the extracted value fails the runtime check,
   *         null if the value cannot be extracted, and
   *         true if the extracted value passes the runtime check
   */
  def fromRuntimeCheckOnConstant[V <: Extractable: Type](runtimeCheck: (v: V) => RuntimeCheck[Nothing])(using Quotes): Expr[false | Null | true] =
    fromRuntimeCheck(Extractable.extract[V].map(runtimeCheck))

  /**
   * Utility method for [[CompileTimeCheck]] macro implementations that evaluates a constraint by attempting to
   * extract constant values from [[Tuple]] type [[T]] and then calling the provided [[RuntimeCheck]] with the values
   * 
   * @param runtimeChecka function returning a [[RuntimeCheck]] when given the values extracted from [[V]]
   * @param Quotes performs operations on expressions
   * @param Tuple.Union[T] <:< Extractable
   * @tparam T the [[Tuple]] type from which to extract values
   * @return an expression that is
   *         false if the extracted values fail the runtime check,
   *         null if any of the values cannot be extracted, and
   *         true if the extracted values pass the runtime check
   */
  def fromRuntimeCheckOnTuple[T <: Tuple : Type](runtimeCheck: (v: T) => RuntimeCheck[Nothing])(using Quotes, Tuple.Union[T] <:< Extractable): Expr[false | Null | true] =
    fromRuntimeCheck(Extractable.extract[Group.FromTuple[T]].map(v => runtimeCheck(v.toTuple.asInstanceOf[T])))

  /**
   * Utility method for [[CompileTimeCheck]] macro implementations that possibly performs a [[RuntimeCheck]] and return
   * the result as an [[Expr]]
   *
   * @param possibleRuntimeCheck maybe a runtime check to perform
   * @param Quotes performs operations on expressions
   * @return an expression that is
   *         false if the runtime check fails,
   *         null if the runtime check is [[None]], and
   *         true if the runtime check passes
   */
  def fromRuntimeCheck(possibleRuntimeCheck: Option[RuntimeCheck[Nothing]])(using Quotes): Expr[false | Null | true] =
    possibleRuntimeCheck match
      case None => '{ null }
      case Some(runtimeCheck) =>
        if runtimeCheck.succeeded
        then '{ true }
        else '{ false }

  /**
   * A check that inlines false (constraint satisfaction invalidated)
   */
  private object False extends CompileTimeCheck[Any]:
    override inline def valid: false = false

  /**
   * A check that inlines null (representing constraint satisfaction being undetermined)
   */
  private object Unknown extends CompileTimeCheck[Any]:
    override inline def valid: Null = null

  /**
   * A check that inlines true (constraint satisfaction validated)
   */
  private object True extends CompileTimeCheck[Any]:
    override inline def valid: true = true

  /**
   * The type class instance for false that invalidates constraints
   * 
   * @return the type class instance [[False]]
   */
  transparent inline given falsehood: CompileTimeCheck[false] = False

  /**
   * The type class instance for [[Null]] that leaves constraint satisfaction unknown
   * 
   * @return the type class instance [[Unknown]]
   */
  transparent inline given unknown: CompileTimeCheck[Null] = Unknown

  /**
   * The type class instance for true that validates constraints
   * 
   * @return the type class instance for [[True]]
   */
  transparent inline given truth: CompileTimeCheck[true] = True

  /**
   * Type class instance of [[CompileTimeCheck]] for [[Not]]
   *
   * @param compileTimeCheck the [[CompileTimeCheck]] instance to negate
   * @tparam C the constraint to negate
   * @return a [[CompileTimeCheck]] for the negation of [[C]]
   *         [[Not]] on false becomes true
   *         [[Not]] on unknown stays unknown
   *         [[Not]] on true becomes false
   */
  transparent inline given [C](using inline compileTimeCheck: CompileTimeCheck[C]): CompileTimeCheck[Not[C]] =
    inline compileTimeCheck.valid match
      case false => True
      case null => Unknown
      case true => False

  /**
   * Type class instance of [[CompileTimeCheck]] for [[and]]
   * 
   * @param a the first [[CompileTimeCheck]] of the conjunction
   * @param b the second [[CompileTimeCheck]] of the conjunction
   * @tparam A the first constraint of the conjunction
   * @tparam B the second constraint of the conjunction
   * @return a [[CompileTimeCheck]] for the conjunction of [[A]] and [[B]]
   *         either being false results in false
   *         neither being false and at least one being unknown results in unknown
   *         both being true results in true
   */
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

  /**
   * Type class instance of [[CompileTimeCheck]] for [[or]]
   * 
   * @param a the first [[CompileTimeCheck]] of the inclusive disjunction
   * @param b the second [[CompileTimeCheck]] of the inclusive disjunction
   * @tparam A the first constraint of the inclusive disjunction
   * @tparam B the second constraint of the inclusive disjunction
   * @return a [[CompileTimeCheck]] for the inclusive disjunction of [[A]] or [[B]]
   *         either being true results in true
   *         neither being true and at least one being unknown results in unknown
   *         both being false results in false
   */
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

  /**
   * Type class instance of [[CompileTimeCheck]] for [[xor]]
   * 
   * @param a the first [[CompileTimeCheck]] of the exclusive disjunction
   * @param b the second [[CompileTimeCheck]] of the exclusive disjunction
   * @tparam A the first constraint of the exclusive disjunction
   * @tparam B the second constraint of the exclusive disjunction
   * @return a [[CompileTimeCheck]] for the exclusive disjunction of [[A]] xor [[B]]
   *         either being unknown results in unknown
   *         both being known results in false if they are the same and true if they are different
   */
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
