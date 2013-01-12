package aima.core.probability

import aima.core.probability.impl.{FiniteRandomVariable, AssignmentProposition}
import scala.annotation.tailrec

package object bayes {
  def sample[A](
    variable: FiniteRandomVariable[A],
    probabilityChoice: Double,
    parentValues: List[AssignmentProposition],
    distribution: List[Double]): A = {
    require(variable.domain.finiteValues.size == distribution.length, s"Size of domain of variable: " +
      s"${variable.domain.finiteValues.size} is not equal to the size of the distribution: ${distribution.length}")
    @tailrec
    def recur(distribution: List[Double], total: Double, index: Int): Int = distribution match {
      case value :: tail if probabilityChoice > total => recur(tail, total + value, index + 1)
      case _ => index
    }
    val index = recur(distribution, 0.0, 0)
    variable.domain.indexedValues(index)
  }
}
