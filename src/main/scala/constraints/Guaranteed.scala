package constraints

/**
 * A value with an attached constraint
 * 
 * @param value the value constrained by [[C]]
 * @tparam V the type being constrained
 * @tparam C the constraint
 */
infix class Guaranteed[+V, C[_]] private(val value: V) extends AnyVal:

  /**
   * Gets the guarantee (which must exist since [[Guaranteed.apply]] needed it to construct this instance)
   *
   * @return the guarantee that the constraint holds
   */
  def guarantee: Guarantee[C[value.type]] = Guarantee.trust

/**
 * Utility methods for constructing [[Guaranteed]] values
 */
object Guaranteed:

  extension [V, C[_]](guaranteed: Guaranteed[V, C])
    def widen[C2[_]](using Normalize[C[V]] <:< Normalize[C2[V]]): Guaranteed[V, C2] =
      new Guaranteed(guaranteed.value)

  /**
   * Constructs a [[Guaranteed]] value with nicer inference than the constructor
   *
   * @param v the value to constrain
   * @param guarantee evidence of the constraint
   * @tparam C the constraint
   * @return the constrained value
   */
  def apply[C[_]](v: Any)(guarantee: Guarantee[C[v.type]]) =
    new Guaranteed[v.type, C](v)

  /**
   * Checks constraint [[C]] on value [[v]] at runtime
   *
   * @param v the value to check
   * @param c the runtime check to run
   * @tparam C the constraint to check
   * @return a constrained value:
   *         if the constraint check fails
   *          - a [[Left]] containing the value and an inverse [[Guarantee]] that the inverse of constraint [[C]] holds
   *         otherwise
   *          - a [[Right]] containing the value and a [[Guarantee]] that constraint [[C]] holds
   */
  def runtimeCheck[C[_]](v: Any)(
    using c: Compute.Typed[C[v.type], Boolean]
  ): Either[v.type Guaranteed Inverse[C], v.type Guaranteed C] =
    Guarantee.testAtRuntime[C[v.type]] match
      case Left(invertedGuarantee: Guarantee[Not[C[v.type]]]) =>
        Left(Guaranteed(v)(invertedGuarantee))
      case Right(guarantee: Guarantee[C[v.type]]) =>
        Right(Guaranteed(v)(guarantee))
