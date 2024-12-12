package constraints

/**
 * Alias for a result of [[test]] which is just an [[Either]] containing a [[Left]] of a [[Not]] [[Guarantee]]
 * or a [[Right]] of a [[Guarantee]] of the constraint [[C]]
 * @tparam C the constraint
 */
type Tested[C] = Either[Guarantee[Not[C]], Guarantee[C]]

/**
 * contextual callback helpers for tested [[Guarantee]]s of constraint [[C]]
 */
extension [C](tested: Tested[C])

  def branch[A](ifHolds: Guarantee[C] ?=> A, ifFails: Guarantee[Not[C]] ?=> A): A = ifElse(ifHolds)(ifFails)

  /**
   * handle either result of the tested constraint [[C]]
   * @param ifGuarantee callback to call if the constraint [[C]] held
   * @param ifInverseGuarantee callback to call if the constraint [[C]] failed
   * @tparam A the return type of the callbacks
   * @return the value resulting from calling the corresponding callback
   */
  def ifElse[A](ifGuarantee: Guarantee[C] ?=> A)(ifInverseGuarantee: Guarantee[Not[C]] ?=> A): A =
    tested.fold(ifInverseGuarantee(using _), ifGuarantee(using _))

  /**
   * Like [[Either.map]] but with a contextual callback
   * @param ifGuarantee the callback to call if constraint [[C]] held
   * @tparam A the return type of the callback
   * @return an [[Either]] with the right side mapped to the callback value if the constraint [[C]] held
   */
  def ifHeld[A](ifGuarantee: Guarantee[C] ?=> A): Either[Guarantee[Not[C]], A] =
    tested.map(ifGuarantee(using _))

  /**
   * Like [[Either.left.map]] but with a contextual callback
   * @param ifInverseGuarantee the callback to call if constraint [[C]] failed
   * @tparam A the return type of the callback
   * @return an [[Either]] with the left side mapped to the callback value if the constraint [[C]] failed
   */
  def ifFailed[A](ifInverseGuarantee: Guarantee[Not[C]] ?=> A): Either[A, Guarantee[C]] =
    tested.left.map(ifInverseGuarantee(using _))

  /**
   * Happy-path chaining of tested [[Guarantee]]s. [[and]]s the resulting constraints together.
   * Prefer [[accumulateWhileFailingWith]] if needing to know each individual failed constraint
   * since this method only [[or]]s the failed constraints together with a single [[Not]] around it.
   * @param ifGuarantee the callback to test the next constraint if this one holds
   * @tparam A the next constraint type
   * @return the collective [[Guarantee]] for or against constraints [[C and A]]
   */
  def chain[A](ifGuarantee: Guarantee[C] ?=> Tested[A]): Tested[C and A] =
    tested.fold(
      Left(_),
      guaranteeOfC =>
        ifGuarantee(using guaranteeOfC)
          .fold(
            Left(_),
            guaranteeOfA => Right(guaranteeOfC and guaranteeOfA)
          )
    )

  /**
   * Falls back to another [[Guarantee]] of the same constraint if this one failed
   * @param ifInverseGuarantee the callback if the constraint [[C]] failed
   * @return the [[Guarantee]] previously acquired if it held, otherwise the result of the callback
   */
  def orElse(ifInverseGuarantee: Guarantee[Not[C]] ?=> Guarantee[C]): Guarantee[C] =
    tested.fold(ifInverseGuarantee(using _), identity)

