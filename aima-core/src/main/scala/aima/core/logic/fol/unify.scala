package aima.core.logic.fol

import annotation.tailrec

object unify {
  def apply(occurCheck: OccurCheck)(left: Sentence, right: Sentence): Option[Substitution] = {
    def unifySentences(left: List[Sentence], right: List[Sentence], θ: Substitution): Option[Substitution] = {
      (left, right) match {
        case (x :: xtail, y :: ytail) => (x, y) match {
          case (a: AtomicSentence, b: AtomicSentence) => unifyTerms(a.terms.to[List], b.terms.to[List], θ)
          case (a: ComplexSentence, b: ComplexSentence) if a hasSameOpAs b => unifySentences(a.args, b.args, θ) match {
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
        case (None, xVar: Variable) if θ contains xVar => unifyTerms(List(variable), List(θ get xVar get), θ)
        case (None, _) => if (occurCheck(variable, x)) None else Some(θ + (variable → x))
      }
    }
    unifySentences(List(left), List(right), Map())
  }
}
