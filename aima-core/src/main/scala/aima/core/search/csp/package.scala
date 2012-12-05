package aima.core.search

/**
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/22/12
 */
package object csp {
  case class NumberedVariable[A](i: Int)
  case class Assignment[A, V[_]](variable: V[A], value: A)
  /**
   * @param values of this domain
   * @tparam A type of this domain
   */
  case class Domain[A](values: Set[A])
  type BinaryRelation[A, B, V[_]] = (Assignment[A, V], Assignment[B, V]) => Boolean
}
