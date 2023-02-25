package constraints

/**
 * Flattens out a constraint of type [[C]] into a chain of [[not]]s, [[and]]s, and [[or]]s
 */
type Normalize[C] = C match
  case not[not[c]] => Normalize[c]
  case not[a and b] => Normalize[not[a] or not[b]]
  case not[a or b] => Normalize[not[a] and not[b]]
  case not[a xor b] => Normalize[not[TranslateXor[a, b]]]
  case a and b => Normalize[a] & Normalize[b]
  case a or b => Normalize[a] | Normalize[b]
  case a xor b => Normalize[TranslateXor[a, b]]
  case not[false] => true
  case not[true] => false
  case _ => C
private type TranslateXor[A, B] = (A and not[B]) or (not[A] and B)
