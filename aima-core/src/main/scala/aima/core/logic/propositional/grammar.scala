package aima.core.logic.propositional

abstract class Sentence {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]): Boolean
  def isFalseIn(model: Map[PropositionSymbol, Boolean]): Boolean
  def symbols: Set[PropositionSymbol]
}

sealed abstract class PropositionSymbol(val symbol: String) extends Sentence {
  require(PropositionSymbol.compiledSymbolPattern.matcher(symbol).matches(), s"Symbol $symbol did not match required " +
    "regex [A-Z][a-z0-9A-Z]* for symbols")
  lazy val symbols: Set[PropositionSymbol] = Set(this)
}
private object PropositionSymbol {
  val compiledSymbolPattern = """[A-Z][a-z0-9A-Z]*""".r.pattern
}
case class PSymbol(override val symbol: String) extends PropositionSymbol(symbol) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]) = model get this getOrElse false
  def isFalseIn(model: Map[PropositionSymbol, Boolean]) = !(model get this getOrElse true)
  override val toString: String = symbol
}
case object True extends PropositionSymbol("True") {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]) = true
  def isFalseIn(model: Map[PropositionSymbol, Boolean]) = false
  override val toString: String = "True"
}
case object False extends PropositionSymbol("False") {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]) = false
  def isFalseIn(model: Map[PropositionSymbol, Boolean]) = true
  override val toString: String = "False"
}

case class AtomicSentence(symbol: PropositionSymbol) extends Sentence {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]): Boolean = symbol isTrueIn model
  def isFalseIn(model: Map[PropositionSymbol, Boolean]) = symbol isFalseIn model
  lazy val symbols: Set[PropositionSymbol] = Set(symbol)
  override val toString: String = symbol.symbol
}

abstract class ComplexSentence(sentences: Traversable[Sentence]) extends Sentence {
  lazy val symbols: Set[PropositionSymbol] = (sentences flatMap {_.symbols}).to[Set]
}
case class ¬(sentence: Sentence) extends ComplexSentence(List(sentence)) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]): Boolean = !(sentence isTrueIn model)
  def isFalseIn(model: Map[PropositionSymbol, Boolean]) = !(sentence isFalseIn model)
  override val toString: String = s"(~$sentence)"
}

sealed abstract class Connective(left: Sentence, right: Sentence) extends ComplexSentence(List(left, right)) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]): Boolean = operator(left isTrueIn model, right isTrueIn model)
  def isFalseIn(model: Map[PropositionSymbol, Boolean]): Boolean = operator(left isFalseIn model, right isFalseIn model)
  def operator(left: Boolean, right: Boolean): Boolean
}
object Connective {
  case class ∧(left: Sentence, right: Sentence) extends Connective(left, right) {
    def operator(left: Boolean, right: Boolean) = left && right
    override val toString: String = s"($left & $right)"
  }
  case class ∨(left: Sentence, right: Sentence) extends Connective(left, right) {
    def operator(left: Boolean, right: Boolean) = left || right
    override val toString: String = s"($left | $right)"
  }
  case class ⇾(premise: Sentence, conclusion: Sentence) extends Connective(premise, conclusion) {
    def operator(left: Boolean, right: Boolean) = (left && right) || !left
    override val toString: String = s"($premise => $conclusion)"
  }
  case class ⇔(left: Sentence, right: Sentence) extends Connective(left, right) {
    def operator(left: Boolean, right: Boolean) = (left && right) || (!left && !right)
    override val toString: String = s"($left <=> $right)"
  }
}

object grammar {
  implicit final class PropositionalLogic(left: Sentence) {
    def ∧(right: Sentence): Connective.∧ = Connective.∧(left, right)
    def ∨(right: Sentence): Connective.∨ = Connective.∨(left, right)
    def ⇾(right: Sentence): Connective.⇾ = Connective.⇾(left, right)
    def ⇔(right: Sentence): Connective.⇔ = Connective.⇔(left, right)
  }

  implicit def booleanToPropositionalSymbol(bool: Boolean): PropositionSymbol = if (bool) True else False
}