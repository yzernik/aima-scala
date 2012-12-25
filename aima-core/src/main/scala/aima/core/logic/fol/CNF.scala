package aima.core.logic.fol

// Clause Normal Form
sealed abstract class Literal(val sentence: AtomicSentence) extends AtomicSentence(sentence.terms)
case class PositiveLiteral(override val sentence: AtomicSentence) extends Literal(sentence)
case class NegativeLiteral(override val sentence: AtomicSentence) extends Literal(sentence)

// Finite conjunction of clauses
case class CNFSentence(clauses: Set[Clause]) extends ComplexSentence(clauses flatMap {_.sentences})

// Finite disjunction of literals
sealed abstract class Clause(val literals: Set[Literal]) extends ComplexSentence(literals map {_.sentence})
case class GeneralClause(override val literals: Set[Literal]) extends Clause(literals)

// Finite disjunction of literals where exactly one is positive
sealed abstract class DefiniteClause(val negLits: Set[NegativeLiteral], val posLit: PositiveLiteral)
  extends Clause(Set[Literal](posLit) ++ negLits)

case class DisjunctionDefinite(override val negLits: Set[NegativeLiteral], override val posLit: PositiveLiteral)
  extends DefiniteClause(negLits, posLit)
case class ImplicationDefinite(premises: Set[PositiveLiteral], conclusion: PositiveLiteral)
  extends DefiniteClause(premises map {literal => NegativeLiteral(literal.sentence)}, conclusion)
case class AtomicDefinite(override val posLit: PositiveLiteral) extends DefiniteClause(Set(), posLit)

object DefiniteClause {
  @inline
  implicit def definiteClauseToDisjunctionDefinite(defClause: DefiniteClause): DisjunctionDefinite = defClause match {
    case x: AtomicDefinite => DisjunctionDefinite(Set(), x.posLit)
    case x: ImplicationDefinite => DisjunctionDefinite(x.negLits, x.posLit)
    case x: DisjunctionDefinite => x
  }
  @inline
  implicit def definiteClauseToImplicationDefinite(defClause: DefiniteClause): ImplicationDefinite = defClause match {
    case x: AtomicDefinite => ImplicationDefinite(Set(PositiveLiteral(Predicate.True)), x.posLit)
    case x: ImplicationDefinite => x
    case DisjunctionDefinite(negLits, posLit) =>
      ImplicationDefinite(negLits map {literal => PositiveLiteral(literal.sentence)}, posLit)
  }
}