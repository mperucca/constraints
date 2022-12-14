package constraints

type Normalize[A] = A match
  case not[not[a]] => Normalize[a]
  case not[a and b] => Normalize[not[a] or not[b]]
  case not[a or b] => Normalize[not[a] and not[b]]
  case not[a xor b] => Normalize[not[TranslateXor[a, b]]]
  case a and b => Normalize[a] & Normalize[b]
  case a or b => Normalize[a] | Normalize[b]
  case a xor b => Normalize[TranslateXor[a, b]]
  case not[false] => true
  case not[true] => false
  case _ => A
private type TranslateXor[A, B] = (A and not[B]) or (not[A] and B)
