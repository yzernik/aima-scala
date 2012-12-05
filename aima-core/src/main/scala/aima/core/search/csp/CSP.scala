package aima.core.search.csp

/**
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/22/12
 */
object CSP {
//case class CSP[V[_]](
//  variables: Set[(V[A], Domain[A]) forSome {type A}], // Set of pairs of variables to domains where the types match.
//  constraints: Set[BinaryConstraint[_, _, V]]) { // Set of binary constraints of where any type fills
////  val cnet: Traversable[(V[A], Set[Constraint[A, _, V[_]]]) forSome {type A}] =
////    (variables map { (variableDomain: (V[A], Domain[A]) forSome {type A}) =>
////      val variable = variableDomain._1
////      (variable, constraints filterNot { constraint => constraint.contains(variable) })
////    })
//
//  lazy val arcs: Traversable[(V[A], V[B], BinaryConstraint[A, B, V]) forSome {type A; type B}] = {
//    variables.map( (variableDomain: (((V[A], Domain[A])) forSome {type A})) => {
//      val variable: V[A] forSome {type A} = variableDomain._1
//      val filtered: Set[BinaryConstraint[A, _, V]] forSome { type A} =
//        (BinaryConstraint.filterConstraints(variable, constraints))
//      true
//      map {
//        constraint =>
//          (variable, constraint.neighborToA(variable), constraint)
//      }
//    })
//    variables flatMap { case (variable, _) => constraints filterNot { constraint => constraint.contains(variable) } map
//      { constraint => (variable, constraint.neighborA(variable).get, constraint) }
//    }
//  }

//  def neighbors[A](variable: V[A]): Set[(V[A], V[_], Constraint[A, _, V[_]])] = {
//    cnet.get(variable).get.map(
//      constraint =>
//        (variable, constraint.neighborA(variable).get, constraint))
//    //    { (constraint: Constraint[A, B, V[_]] forSome {type B}) =>
//    //      val myVariable: V[A] = variable
//    //      val myNeighbor: V[B] = constraint.neighbor(variable).get
//    //      (variable, constraint.neighbor(variable).get, constraint) }
//  }
//
//  def clone(variableDomains: (V, Domain[_])*): CSP[V] = {
//    this.copy(variables = this.variables ++ variableDomains)
//  }
}