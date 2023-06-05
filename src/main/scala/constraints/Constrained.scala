package constraints

/**
 * A value with an attached constraint
 * 
 * @param value the value constrained by [[C]]
 * @tparam V the type being constrained
 * @tparam C the constraint
 */
infix class Constrained[+V, C[_]] private(val value: V) extends AnyVal:

  /**
   * Gets the guarantee (which must exist since [[Constrained.apply]] needed it to construct this instance)
   * 
   * @return the guarantee that the constraint holds
   */
  def guarantee: Guarantee[C[value.type]] = Guarantee.trust

/**
 * Utility methods for constructing [[Constrained]] values
 */
object Constrained:

  /**
   * Constructs a [[Constrained]] value with nicer inference than the constructor
   * 
   * @param v the value to constrain
   * @param guarantee evidence of the constraint
   * @tparam C the constraint
   * @return the constrained value
   */
  def apply[C[_]](v: Any)(guarantee: Guarantee[C[v.type]]) =
    new Constrained[v.type, C](v)

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
    using c: Compute.Predicate[C[v.type]]
  ): Either[v.type Constrained Inverse[C], v.type Constrained C] =
    Guarantee.testAtRuntime[C[v.type]] match
      case Left(invertedGuarantee: Guarantee[Not[C[v.type]]]) =>
        Left(Constrained(v)(invertedGuarantee))
      case Right(guarantee: Guarantee[C[v.type]]) =>
        Right(Constrained(v)(guarantee))
