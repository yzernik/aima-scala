package aima.core.logic.fol

import aima.core.logic.fol.grammar._
import aima.core.logic.fol.Connective._

// Need to rework to ensure renaiming isnt the same as before
object standardize extends Standardize {
  // Implications will be converted to disjunctions
  def apply(standardizeQV: StandardizeQV)(sentence: Sentence): Sentence = sentence match {
    case x: AtomicSentence => x
    case ¬(x) => ¬(standardize(x))
    case left ∨ right =>
      standardizeQV(Set(left, right)) reduce {(sLeft, sRight) => standardize(left) ∨ standardize(right)}
    case left ∧ right =>
      standardizeQV(Set(left, right)) reduce {(sLeft, sRight) => standardize(left) ∧ standardize(right)}
    case premise ⇾ conclusion => standardize(¬(premise) ∨ conclusion)
    case left ⇔ right =>
      standardizeQV(Set(left, right)) reduce {(sLeft, sRight) => standardize(left) ⇔ standardize(right)}
    case ∀(variable, s) => ∀(variable, standardize(s))
    case ∃(variable, s) => ∃(variable, standardize(s))
  }
  def apply(sentence: Sentence): Sentence = standardize(standardizeQV)(sentence)
}
