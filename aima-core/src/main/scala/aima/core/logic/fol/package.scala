package aima.core.logic

package object fol {
  type Substitution = Map[Variable, Term]
  type Unifier = (Sentence, Sentence) => Option[Substitution]
  trait Substitute {
    def apply[T](θ: Substitution, α: T): T
  }
  // Converts any sentence to CNF
  type SentenceToCNF = Sentence => CNFSentence
  // Standardizes variables given in the set of sentences for one level (i.e., not recursively, see standardize)
  type StandardizeQV = Set[Sentence] => Set[Sentence]
  // Standardizes recursively any sentence made from grammar
  type Standardize = Sentence => Sentence
  // Produces a unique variable from the given variable, thread-safe
  type StandardVariable = Variable => Variable
  // Produces a unique function given the set of terms, thread-safe
  type StandardFunction = Set[Term] => AFunction
}
