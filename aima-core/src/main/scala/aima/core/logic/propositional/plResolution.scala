package aima.core.logic.propositional

import annotation.tailrec
import aima.core.logic.propositional.grammar.PropositionalLogic

object plResolution {
  def apply(sentenceToCNF: SentenceToCNF, plResolve: PLResolve)(kb: KnowledgeBase[_], α: Sentence): Boolean = {
    @tailrec
    def recur(clauses: Set[Clause]): Boolean = {
      @tailrec
      def recur2(pairs: List[(Clause, Clause)], newSet: Set[Clause]): Option[Set[Clause]] = pairs match {
        case (left, right) :: tail =>
          val resolvents = plResolve(left, right)
          if (resolvents.exists(_.literals.isEmpty)) None else recur2(tail, newSet ++ resolvents)
        case Nil => Some(newSet)
      }
      val pairs = clauses.to[Seq] combinations 2 map {case Seq(left, right) => (left, right)}
      recur2(pairs.to[List], Set()) match {
        case Some(s) if s.subsetOf(clauses) => false
        case Some(s) => recur(clauses ++ s)
        case None => true
      }
    }
    recur(sentenceToCNF(kb ∧ ¬(α)).clauses)
  }
}
