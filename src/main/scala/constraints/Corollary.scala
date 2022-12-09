package constraints

sealed trait Corollary[-A, +L]

object Corollary extends Corollary[Any, Nothing]:
  
  def apply[A, L]: Corollary[A, L] = Corollary
