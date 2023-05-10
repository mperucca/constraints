package constraints

import quoted.*

/**
 * Utility methods and type class instances for provided combinators
 */
object CompileTimeCheck:

  type Typed[-C] = CompileTimeComputation.Typed[C, Boolean]

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
   * @param runtimeCheck a function returning a [[RuntimeCheck]] when given the values extracted from [[V]]
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
