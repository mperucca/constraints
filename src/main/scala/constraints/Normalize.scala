package constraints

/**
 * Flattens out a constraint of type [[C]] into a chain of [[Not]]s, [[and]]s, and [[or]]s
 * @note Although [[xor]] is not implemented in terms of [[Not]], [[and]], and [[or]], this still normalizes it as such.
 */
type Normalize[C] = C match
  case Not[Not[c]] => Normalize[c]
  case Not[a and b] => Normalize[Not[a] or Not[b]]
  case Not[a or b] => Normalize[Not[a] and Not[b]]
  case Not[a xor b] => Normalize[Not[RewriteXor[a, b]]]
  case a and b => Normalize[a] & Normalize[b]
  case a or b => Normalize[a] | Normalize[b]
  case a xor b => Normalize[RewriteXor[a, b]]
  case Not[false] => true
  case Not[true] => false
  case _ => C
private type RewriteXor[A, B] = (A and Not[B]) or (Not[A] and B)
