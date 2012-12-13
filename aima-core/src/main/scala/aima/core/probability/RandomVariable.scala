package aima.core.probability

case class RandomVariable[A](num: Int, domain: Domain[A]) {
  val values = domain.values

  override def equals(other: Any): Boolean = other match {
    case that: RandomVariable[A] => (that canEqual this) && that.num == this.num
    case _ => false
  }

  override def hashCode(): Int = num.hashCode()
}
