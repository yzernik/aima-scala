package aima.core.logic.fol

import aima.core.logic.fol.Connective._
import aima.core.logic.fol.grammar.FOLLogic
import aima.core.logic.fol.Defaults.defaultStandardFunction

object sentenceToCNF extends SentenceToCNF {
  def apply(sub: Substitute, standardize: Standardize)
    (sentence: Sentence)
    (implicit standardFunction: StandardFunction): CNFSentence = {
    def removeImplications(sentence: Sentence): Sentence = sentence match {
      case x: AtomicSentence => x
      case ¬(x) => ¬(removeImplications(x))
      case left ∨ right => removeImplications(left) ∨ removeImplications(right)
      case left ∧ right => removeImplications(left) ∧ removeImplications(right)
      case premise ⇾ conclusion => ¬(removeImplications(premise)) ∨ removeImplications(conclusion)
      case left ⇔ right => removeImplications((left ⇾ right) ∧ (right ⇾ left))
      case ∀(symbol, s) => ∀(symbol, removeImplications(s))
      case ∃(symbol, s) => ∃(symbol, removeImplications(s))
    }

    def negationsIn(sentence: Sentence): Sentence = sentence match {
      case x: AtomicSentence => x
      case ¬(x) => negationsIn(grammar.negate(x))
      case left ∨ right => negationsIn(left) ∨ negationsIn(right)
      case left ∧ right => negationsIn(left) ∧ negationsIn(right)
      case ∀(symbol, s) => ∀(symbol, negationsIn(s))
      case ∃(symbol, s) => ∃(symbol, negationsIn(s))
    }

    def removeExistentials(sentence: Sentence): Sentence = {
      def collectSentenceFreeVariables(bound: Set[Variable], sentence: Sentence): Set[Variable] = sentence match {
        case x: AtomicSentence => x.terms flatMap {
          case a: Variable if bound(a) => Some(a)
          case _ => None
        }
        case ¬(x) => collectSentenceFreeVariables(bound, x)
        case left ∨ right => collectSentenceFreeVariables(bound, left) ++ collectSentenceFreeVariables(bound, right)
        case left ∧ right => collectSentenceFreeVariables(bound, left) ++ collectSentenceFreeVariables(bound, right)
        case x: Quantifier => collectSentenceFreeVariables(bound + x.variable, x.sentence)
      }
      sentence match {
        case x: AtomicSentence => x
        case ¬(x) => ¬(removeExistentials(x))
        case left ∨ right => removeExistentials(left) ∨ removeExistentials(right)
        case left ∧ right => removeExistentials(left) ∧ removeExistentials(right)
        case ∀(variable, s) => ∀(variable, removeExistentials(s))
        case ∃(variable, s) =>
          val freeVariables: Set[Term] = collectSentenceFreeVariables(Set(), s).toSet
          sub(Map(variable → standardFunction(freeVariables)), sentence)
      }
    }

    def removeUniversals(sentence: Sentence): Sentence = sentence match {
      case x: AtomicSentence => x
      case ¬(x) => ¬(removeUniversals(x))
      case left ∨ right => removeUniversals(left) ∨ removeUniversals(right)
      case left ∧ right => removeUniversals(left) ∧ removeUniversals(right)
      case ∀(variable, s) => removeUniversals(s)
    }

    def distrib(sentence: Sentence): Set[Clause] = sentence match {
      case x: AtomicSentence => Set(GeneralClause(Set(PositiveLiteral(x))))
      case ¬(x: AtomicSentence) => Set(GeneralClause(Set(NegativeLiteral(x))))
      case left ∧ right => distrib(left) ++ distrib(right)
      case left ∨ right =>
        val leftClauses = distrib(left)
        val rightClauses = distrib(right)
        leftClauses flatMap
          {lclause => rightClauses map {rclause => GeneralClause(lclause.literals ++ rclause.literals)}}
    }

    val eliminateImplications = removeImplications(sentence)
    val moveNotInwards = negationsIn(eliminateImplications)
    val standardizeVariables = standardize(moveNotInwards)
    val skolemized = removeExistentials(standardizeVariables)
    val droppedUniversals = removeUniversals(skolemized)
    val distributed = distrib(droppedUniversals)
    CNFSentence(distributed)
  }
  def apply(sentence: Sentence): CNFSentence = sentenceToCNF(substitute, standardize)(sentence)
}
