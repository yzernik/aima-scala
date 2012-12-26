package aima.core.logic.fol

import scala.annotation.tailrec

object folBCAsk {
  def apply(kb: KnowledgeBase, α: AtomicSentence): Set[Substitution] = {
    folBCOr(kb, α, Map())
  }

  def folBCOr(kb: KnowledgeBase, goal: AtomicSentence, θ: Substitution): Set[Substitution] = {
    @tailrec
    def recur(rules: List[ImplicationDefinite], partial: Set[Substitution]): Set[Substitution] = rules match {
      case rule :: tail =>
        val standardized = standardizeSentence(rule)
        val (lhs, rhs) = (standardized.premises.toList map {_.sentence}, standardized.conclusion.sentence)
        recur(tail, partial ++ folBCAnd(kb, lhs, unify(rhs, goal, θ)))
      case Nil => partial
    }
    val rulesForGoal = kb.implicationDefinites filter {clause => unify(clause.conclusion.sentence, goal).isDefined}
    recur(rulesForGoal.toList, Set())
  }

  def folBCAnd(kb: KnowledgeBase, goals: List[AtomicSentence], θ: Option[Substitution]): Set[Substitution] =
    (θ, goals) match {
      case (None, _) => Set()
      case (Some(substitution), Nil) => Set(substitution)
      case (Some(substitution), first :: rest) =>
        folBCOr(kb, substitute(substitution, first), substitution) flatMap {sub => folBCAnd(kb, rest, Some(sub))}
    }
}
