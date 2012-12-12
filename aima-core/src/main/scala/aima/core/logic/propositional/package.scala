package aima.core.logic

package object propositional {
  // Returns set of all possible clauses obtained by resolving the two inputs
  type PLResolve = (Clause, Clause) => Set[Clause]
  type SentenceToCNF = Sentence => CNFSentence
}
