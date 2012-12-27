package aima.core.logic.propositional

object ttEntails {
  def apply(kb: KnowledgeBase[_], α: Sentence): Boolean = {
    def ttCheckAll(symbols: List[PropositionSymbol], model: Map[PropositionSymbol,
      Boolean]): Boolean = symbols match {
      case symbol :: tail => ttCheckAll(tail, model + (symbol → true)) && ttCheckAll(tail, model + (symbol → false))
      case Nil => if (kb.asSentence isTrueIn model) α isTrueIn model else true
    }
    ttCheckAll((kb.asSentence.symbols ++ α.symbols).to[List], Map())
  }
}
