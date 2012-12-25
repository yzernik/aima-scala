package aima.core.logic.fol

import aima.core.logic.fol.Defaults.defaultStandardVariable

object standardizeQV extends StandardizeQV {
  // Standardizes between the sentences in the sets, not recursively.
  def apply(sub: Substitute)
    (sentences: Set[Sentence])
    (implicit standardVariable: StandardVariable): Set[Sentence] = {
    val groupedSentences = sentences groupBy {
      case x: Quantifier => true
      case _ => false
    }

    val sameVarSentences = (groupedSentences.getOrElse(true, Set()) foldLeft Map[Variable, Set[Quantifier]]()) {
      case (sameVar, α: Quantifier) => sameVar + (α.variable → (sameVar.getOrElse(α.variable, Set()) + α))
    }

    val standardizedSentences = sameVarSentences flatMap {case (_, quantifiers) =>
      quantifiers.map[Quantifier, Set[Quantifier]] {
        case ∀(variable, s) =>
          val newVariable = Variable(standardVariable(variable))
          ∀(newVariable, sub(Map(variable → newVariable), s))
        case ∃(variable, s) =>
          val newVariable = Variable(standardVariable(variable))
          ∃(newVariable, sub(Map(variable → newVariable), s))
      }
    }

    groupedSentences.getOrElse(false, Set()) ++ standardizedSentences
  }
  def apply(sentences: Set[Sentence]) = standardizeQV(substitute)(sentences)
}
