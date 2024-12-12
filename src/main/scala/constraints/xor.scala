package constraints

import scala.annotation.targetName

/**
 * Exclusive disjunction (sometimes represented by the symbol ⊕)
 *
 * @note Even though this could be a typed as {{{infix type xor[A, B] = (A and Not[B]) or (Not[A] and B)}}},
 *       using a separate trait means the implementation takes half the runtime checks should it fail
 * @tparam A the first term of the disjunction
 * @tparam B the second term of the disjunction
 */
infix sealed trait xor[A, B]

@targetName("xor")
type ^[A, B] = A xor B

object xor extends Compute.BinaryCompanion[^, Boolean, Boolean, Boolean](_ ^ _)
