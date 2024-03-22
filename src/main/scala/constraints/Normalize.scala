package constraints

/**
 * Flattens out a constraint of type [[C]] into a chain of [[Not]]s, [[and]]s, and [[or]]s
 * @note Although [[xor]] is not implemented in terms of [[Not]], [[and]], and [[or]], this still normalizes it as such.
 */
type Normalize[C] = C match
  case Not[a] => RewriteNot[a]
  case a and b => Normalize[a] & Normalize[b]
  case a or b => Normalize[a] | Normalize[b]
  case a xor b => Normalize[RewriteXor[a, b]]
  case Not[false] => true
  case Not[true] => false
  case _ => C
private type RewriteNot[A] = A match
  case false => true
  case true => false
  case Not[a] => Normalize[a]
  case a and b => Normalize[Not[a] or Not[b]]
  case a or b => Normalize[Not[a] and Not[b]]
  case a xor b => Normalize[Not[RewriteXor[a, b]]]
  case _ => Not[A]
private type RewriteXor[A, B] = (A and Not[B]) or (Not[A] and B)
