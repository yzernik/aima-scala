package aima.core.search.csp

case class Variable[A](num: Int, domain: Set[A]) {
  override def equals(other: Any): Boolean = other match {
    case that: Variable[A] => (that canEqual this) && that.num == this.num
    case _ => false
  }

  override def hashCode(): Int = num.hashCode()
}