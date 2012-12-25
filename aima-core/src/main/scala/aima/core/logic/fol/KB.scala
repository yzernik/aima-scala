package aima.core.logic.fol

import aima.core.logic.fol.DefiniteClause.definiteClauseToImplicationDefinite

final class KB(
  val sentences: Set[Sentence],
  val clauses: Set[Clause],
  val definiteClauses: Set[DefiniteClause],
  val predicates: Map[String, Set[Predicate]],
  val equalities: Set[TermEqual],
  val convertToCNF: SentenceToCNF,
  val unifier: Unifier) extends KnowledgeBase {self =>
  val implicationDefinites: Set[ImplicationDefinite] = definiteClauses map definiteClauseToImplicationDefinite

  def ask(sentence: Sentence): InferenceResult = ???

  protected def store(sentence: Sentence): KB = {
    val cnf = convertToCNF(sentence)
    if (!sentences(sentence) && !(cnf.clauses forall clauses)) {
      val newSentences = sentences + sentence
      val newClauses = clauses ++ cnf.clauses
      val cnfDefiniteClauses = cnf.clauses flatMap {
        case x: DefiniteClause => Some(x)
        case _ => None
      }
      val newDefiniteClauses = definiteClauses ++ cnfDefiniteClauses
      val newAtoms = cnfDefiniteClauses flatMap {
        case x: AtomicDefinite => Some(x)
        case _ => None
      }
      val newPredicates = predicates ++ (newAtoms flatMap {
        _.posLit.sentence match {
          case x: Predicate => Some(x.symbol → (predicates.getOrElse(x.symbol, Set()) + x))
          case x: TermEqual => None
        }
      })
      val newEqualities = equalities ++ (newAtoms flatMap {
        _.posLit.sentence match {
          case x: Predicate => None
          case x: TermEqual => Some(x)
        }
      })
      new KB(newSentences, newClauses, newDefiniteClauses, newPredicates, newEqualities, convertToCNF, unifier)
    } else {
      self
    }
  }

  def fetch(literal: Literal): Set[Substitution] = literal.sentence match {
    case Predicate(symbol, terms) => predicates.get(symbol) match {
      case Some(matchingPredicates) => matchingPredicates flatMap {unifier(literal.sentence, _)}
      case None => Set()
    }
    case x: TermEqual => equalities flatMap {unifier(x, _)}
  }

  def fetch(literals: List[Literal]): Set[Substitution] = {
    def converge(subs1: Set[Substitution], subs2: Set[Substitution]): Set[Substitution] = {
      def merge(sub1: Map[Variable, Term], sub2: Map[Variable, Term]): Option[Map[Variable, Term]] = {
        val conflict = sub1 exists {case (variable, term) => (sub2 contains variable) && sub2(variable) != term}
        if (conflict) None else Some(sub1 ++ sub2)
      }
      subs1 flatMap {sub1 => subs2 flatMap {merge(sub1, _)}}
    }
    literals map fetch reduce converge
  }
}