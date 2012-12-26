package aima.core.logic

package object fol {
  type Substitution = Map[Variable, Term]
  // Produces a unique variable symbol from the given variable, thread-safe
  type StandardVariable = Variable => String
  // Produces a unique function given the set of terms, thread-safe
  type StandardFunction = Set[Term] => AFunction

  object Defaults extends FOLDefaults
}
