package aima.core.logic

package object fol {
  type OccurCheck = (Variable, Term) => Boolean
  type Substitution = Map[Variable, Term]
}
