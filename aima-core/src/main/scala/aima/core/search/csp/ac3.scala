package aima.core.search.csp

import scala.annotation._
import scala.Some

/**
 * Artificial Intelligence A Modern Approach (3rd Ed.): Figure 6.3, Page 209.<br>
 * <br>
 *
 * <pre>
 * <code>
 * function AC-3(csp) returns false if an inconsistency is found and true otherwise
 * inputs: csp, a binary CSP with components (X, D, C)
 * local variables: queue, a queue of arcs, initially all the arcs in csp
 * while queue is not empty do
 * (Xi, Xj) = REMOVE-FIRST(queue)
 * if REVISE(csp, Xi, Xj) then
 * if size of Di = 0 then return false
 * for each Xk in Xi.NEIGHBORS - {Xj} do
 * add (Xk, Xi) to queue
 * return true
 *
 * function REVISE(csp, Xi, Xj) returns true iff we revise the domain of Xi
 * revised = false
 * for each x in Di do
 * if no value y in Dj allows (x ,y) to satisfy the constraint between Xi and Xj then
 * delete x from Di
 * revised = true
 * return revised
 * </code>
 * </pre>
 *
 * Figure 6.3 The arc-consistency algorithm AC-3. After applying AC-3, either
 * every arc is arc-consistent, or some variable has an empty domain, indicating
 * that the CSP cannot be solved. The name "AC-3" was used by the algorithm's
 * inventor (Mackworth, 1977) because it's the third version developed in the
 * paper.
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/22/12
 */
object ac3 {
//  def apply[V](csp: CSP[V]): Option[CSP[V]] = {
//    @tailrec
//    def recur(queue: Traversable[(V, V, Constraint[V])], csp: CSP[V]): Option[CSP[V]] = queue match {
//      case (xi, xj, constraint) :: tail =>
//        revise(csp, xi, xj, constraint) match {
//          case Some(domain) if domain.isEmpty => None
//          case Some(domain) =>
//            val newCSP = csp.copy(variables = csp.variables + (xi → Domain(domain)))
//            val newQueue = tail ++ (csp.neighbors(xi) filterNot { _._1 == xj })
//            recur(newQueue, newCSP)
//          case None => recur(tail, csp)
//        }
//      case Nil => Some(csp)
//    }
//    recur(csp.arcs, csp)
//  }
//
//  def revise[V](csp: CSP[V], xi: V, xj: V, constraint: Constraint[V]): Option[Set[_]] = {
//    val iValues = csp.variables.get(xi) map { _.values } getOrElse Set()
//    val jValues = csp.variables.get(xj) map { _.values } getOrElse Set()
//    val consistentDomainXi = iValues filter
//      { iValue => jValues exists { jValue => constraint.rel(Map(xi -> iValue, xj -> jValue)) } }
//    if (consistentDomainXi.size != iValues.size) Some(consistentDomainXi) else None
//  }
}
