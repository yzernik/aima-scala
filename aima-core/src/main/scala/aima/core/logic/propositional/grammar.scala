package aima.core.logic.propositional

abstract class Sentence {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]): Boolean
  def isFalseIn(model: Map[PropositionSymbol, Boolean]): Boolean
  def symbols: Set[PropositionSymbol]
}

sealed abstract class PropositionSymbol(val symbol: String) extends Sentence {
  lazy val symbols: Set[PropositionSymbol] = Set(this)
}
case class PSymbol(override val symbol: String) extends PropositionSymbol(symbol) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]) = model get this getOrElse false
  def isFalseIn(model: Map[PropositionSymbol, Boolean]) = !(model get this getOrElse true)
}
case object True extends PropositionSymbol("True") {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]) = true
  def isFalseIn(model: Map[PropositionSymbol, Boolean]) = false
}
case object False extends PropositionSymbol("False") {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]) = false
  def isFalseIn(model: Map[PropositionSymbol, Boolean]) = true
}

case class AtomicSentence(symbol: PropositionSymbol) extends Sentence {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]): Boolean = symbol isTrueIn model
  def isFalseIn(model: Map[PropositionSymbol, Boolean]) = symbol isFalseIn model
  lazy val symbols: Set[PropositionSymbol] = Set(symbol)
}

abstract class ComplexSentence(sentences: Traversable[Sentence]) extends Sentence {
  lazy val symbols: Set[PropositionSymbol] = (sentences flatMap {_.symbols}).to[Set]
}
case class ¬(sentence: Sentence) extends ComplexSentence(List(sentence)) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]): Boolean = !(sentence isTrueIn model)
  def isFalseIn(model: Map[PropositionSymbol, Boolean]) = !(sentence isFalseIn model)
}

sealed abstract class Connective(left: Sentence, right: Sentence) extends ComplexSentence(List(left, right)) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]): Boolean = operator(left isTrueIn model, right isTrueIn model)
  def isFalseIn(model: Map[PropositionSymbol, Boolean]): Boolean = operator(left isFalseIn model, right isFalseIn model)
  def operator(left: Boolean, right: Boolean): Boolean
}
case class ∧(left: Sentence, right: Sentence) extends Connective(left, right) {
  def operator(left: Boolean, right: Boolean) = left && right
}
case class ∨(left: Sentence, right: Sentence) extends Connective(left, right) {
  def operator(left: Boolean, right: Boolean) = left || right
}
case class ⇾(premise: Sentence, conclusion: Sentence) extends Connective(premise, conclusion) {
  def operator(left: Boolean, right: Boolean) = (left && right) || !left
}
case class ⇔(left: Sentence, right: Sentence) extends Connective(left, right) {
  def operator(left: Boolean, right: Boolean) = (left && right) || (!left && !right)
}