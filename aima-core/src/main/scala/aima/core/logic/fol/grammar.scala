package aima.core.logic.fol

abstract class Sentence

sealed abstract class AtomicSentence(val terms: Set[Term]) extends Sentence
case class Predicate(symbol: String, override val terms: Set[Term]) extends AtomicSentence(terms)
case class TermEqual(left: Term, right: Term) extends AtomicSentence(Set(left, right))

sealed abstract class ComplexSentence(sentences: Set[Sentence]) extends Sentence {
  def args: List[Sentence] = sentences.to[List]
  def hasSameOpAs(other: ComplexSentence): Boolean
}
case class ¬(sentence: Sentence) extends ComplexSentence(Set(sentence)) {
  def hasSameOpAs(other: ComplexSentence): Boolean = other.isInstanceOf[¬]
}

sealed abstract class Connective(left: Sentence, right: Sentence) extends ComplexSentence(Set(left, right))
case class ∧(left: Sentence, right: Sentence) extends Connective(left, right) {
  def hasSameOpAs(other: ComplexSentence): Boolean = other.isInstanceOf[∧]
}
case class ∨(left: Sentence, right: Sentence) extends Connective(left, right) {
  def hasSameOpAs(other: ComplexSentence): Boolean = other.isInstanceOf[∨]
}
case class ⇾(premise: Sentence, conclusion: Sentence) extends Connective(premise, conclusion) {
  def hasSameOpAs(other: ComplexSentence): Boolean = other.isInstanceOf[⇾]
}
case class ⇔(left: Sentence, right: Sentence) extends Connective(left, right) {
  def hasSameOpAs(other: ComplexSentence): Boolean = other.isInstanceOf[⇔]
}

case class QuantifiedSentence(quantifier: Quantifier, symbols: Set[Variable], sentence: Sentence) extends ComplexSentence(Set(sentence)) {
  override def hasSameOpAs(other: ComplexSentence): Boolean = other match {
    case that: QuantifiedSentence => that.quantifier == this.quantifier
    case _ => false
  }
}

sealed abstract class Quantifier
case object ∀ extends Quantifier
case object ∃ extends Quantifier

sealed abstract class Term(val symbol: String)
case class Function(override val symbol: String, terms: Set[Term]) extends Term(symbol)
case class Constant(override val symbol: String) extends Term(symbol)
case class Variable(override val symbol: String) extends Term(symbol)