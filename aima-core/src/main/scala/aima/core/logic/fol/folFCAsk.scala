package aima.core.logic.fol

import scala.annotation.tailrec

object folFCAsk {
  def apply(sub: Substitute, unify: Unifier)
    (kb: KnowledgeBase, α: Sentence)
    (implicit standardVariable: StandardVariable): Option[Substitution] = {
    def fetch(s: AtomicSentence, set: Set[AtomicSentence]): Set[Map[Variable, Term]] = set flatMap {unify(s, _)}
    def invert[T <: Literal](literals: Set[T]): Set[Literal] = literals map {
      case PositiveLiteral(s) => NegativeLiteral(s)
      case NegativeLiteral(s) => PositiveLiteral(s)
    }
    def standardizeVariables(clause: ImplicationDefinite): ImplicationDefinite = {
      val θ: Map[Variable, Variable] =
        (collectVariables(CNFSentence(Set(clause))) map {v => v -> Variable(standardVariable(v))})(collection.breakOut)
      ImplicationDefinite(sub(θ, clause.premises), sub(θ, clause.conclusion))
    }

    @tailrec
    def recur(clauses: List[ImplicationDefinite], newSet: Set[AtomicSentence]): Option[Substitution] = clauses match {
      case clause :: tail =>
        val standardized = standardizeVariables(clause)
        @tailrec
        def recur2(
          thetas: List[Substitution],
          newSet: Set[AtomicSentence]): (Set[AtomicSentence], Option[Substitution]) = thetas match {
          case θ :: tail2 =>
            val qPrime = sub(θ, standardized.conclusion)
            if (kb.fetch(qPrime).isEmpty && fetch(qPrime, newSet).isEmpty) {
              unify(qPrime, α) match {
                case Some(substitution) => (newSet, Some(substitution))
                case None => recur2(tail2, newSet + qPrime.sentence)
              }
            } else {
              recur2(tail2, newSet)
            }
          case Nil => (newSet, None)
        }
        recur2(kb.fetch(invert(standardized.premises).toList).toList, newSet) match {
          case (resultNewSet, Some(substitution)) => Some(substitution)
          case (resultNewSet, None) => recur(tail, resultNewSet)
        }
      case Nil if newSet.isEmpty => None
      case Nil =>
        kb.tell(newSet)
        recur(kb.implicationDefinites.toList, Set())
    }
    recur(kb.implicationDefinites.toList, Set())
  }
}
