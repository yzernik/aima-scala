package aima.core.logic

package object propositional {
  type Model = Map[PropositionSymbol, Boolean]

  // Returns set of all possible clauses obtained by resolving the two inputs
  type PLResolve = (Clause, Clause) => Set[Clause]
  type SentenceToCNF = Sentence => CNFSentence
  type FindPureSymbol = (Set[Clause], Set[PropositionSymbol], Model) => Option[(PropositionSymbol, Boolean)]
  type FindUnitClause = (Set[Clause], Model) => Option[(PropositionSymbol, Boolean)]
}
