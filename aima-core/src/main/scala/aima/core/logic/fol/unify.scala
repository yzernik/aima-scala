package aima.core.logic.fol

import aima.core.logic.fol.Connective._
import scala.annotation.tailrec

object unify {
  type OccurCheck = (Variable, Term) => Boolean

  def apply(left: Sentence, right: Sentence): Option[Substitution] =
    unify(left, right, Map())

  // Default implementation ommits occurCheck
  def apply(left: Sentence, right: Sentence, θ: Substitution): Option[Substitution] =
    unify((variable, term) => false)(left, right, θ)

  def apply(occurCheck: OccurCheck)(left: Sentence, right: Sentence, θ: Substitution): Option[Substitution] = {
    def hasSameOp(a: ComplexSentence, b: ComplexSentence): Boolean = (a, b) match {
      case (_: ¬, _: ¬) => true
      case (_: ∨, _: ∨) => true
      case (_: ∧, _: ∧) => true
      case (_: ⇾, _: ⇾) => true
      case (_: ⇔, _: ⇔) => true
      case (_: ∀, _: ∀) => true
      case (_: ∃, _: ∃) => true
      case _ => false
    }
    def unifySentences(left: List[Sentence], right: List[Sentence], θ: Substitution): Option[Substitution] = {
      (left, right) match {
        case (x :: xtail, y :: ytail) => (x, y) match {
          case (a: AtomicSentence, b: AtomicSentence) => unifyTerms(a.terms.to[List], b.terms.to[List], θ)
          case (a: ComplexSentence, b: ComplexSentence) if hasSameOp(a, b) => unifySentences(a.args, b.args, θ) match {
            case Some(substitution) => unifySentences(xtail, ytail, substitution)
            case None => None
          }
          case _ => None
        }
        case (Nil, Nil) => Some(θ)
        case _ => None
      }
    }
    @tailrec
    def unifyTerms(left: List[Term], right: List[Term], θ: Substitution): Option[Substitution] = {
      (left, right) match {
        case (x :: xtail, y :: ytail) => (x, y) match {
          case (a: Variable, _) => unifyVars(a, y, θ)
          case (_, a: Variable) => unifyVars(a, y, θ)
          case (Function(_, xterms), Function(_, yterms)) => unifyTerms(xterms.to[List], yterms.to[List], θ)
          case _ => None
        }
        case (Nil, Nil) => Some(θ)
        case _ => None
      }
    }
    def unifyVars(variable: Variable, x: Term, θ: Substitution): Option[Substitution] = {
      (θ get variable, x) match {
        case (Some(value), _) => unifyTerms(List(value), List(x), θ)
        case (None, xVar: Variable) if θ contains xVar => unifyTerms(List(variable), List(θ(xVar)), θ)
        case (None, _) => if (occurCheck(variable, x)) None else Some(θ + (variable → x))
      }
    }
    unifySentences(List(left), List(right), θ)
  }

  def merge(sub1: Map[Variable, Term], sub2: Map[Variable, Term]): Option[Map[Variable, Term]] = {
    val conflict = sub1 exists {case (variable, term) => (sub2 contains variable) && sub2(variable) != term}
    if (conflict) None else Some(sub1 ++ sub2)
  }
}
