package constraints

/**
 * Flattens out a constraint of type [[C]] into a chain of [[Not]]s, [[and]]s, and [[or]]s
 */
type Normalize[C] = C match
  case Not[Not[c]] => Normalize[c]
  case Not[a and b] => Normalize[Not[a] or Not[b]]
  case Not[a or b] => Normalize[Not[a] and Not[b]]
  case Not[a xor b] => Normalize[Not[TranslateXor[a, b]]]
  case a and b => Normalize[a] & Normalize[b]
  case a or b => Normalize[a] | Normalize[b]
  case a xor b => Normalize[TranslateXor[a, b]]
  case Not[false] => true
  case Not[true] => false
  case _ => C
private type TranslateXor[A, B] = (A and Not[B]) or (Not[A] and B)
