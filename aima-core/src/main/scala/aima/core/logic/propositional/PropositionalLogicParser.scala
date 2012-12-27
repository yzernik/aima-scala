package aima.core.logic.propositional

import aima.core.logic.propositional.grammar.PropositionalLogic
import scala.util.parsing.combinator.JavaTokenParsers

object PropositionalLogicParser extends JavaTokenParsers {
  def sentence: Parser[Sentence] = biconditional

  def biconditional: Parser[Sentence] = conditional~opt("<=>"~conditional) ^^ {
    case left~None => left
    case left~Some(_~right) => left ⇔ right
  }

  def conditional: Parser[Sentence] = disjunction~opt("=>"~disjunction) ^^ {
    case premise~None => premise
    case premise~Some(_~conclusion) => premise ⇾ conclusion
  }

  def disjunction: Parser[Sentence] = conjunction~opt("|"~conjunction) ^^ {
    case left~None => left
    case left~Some(_~right) => left ∨ right
  }

  def conjunction: Parser[Sentence] = negation~opt("&"~negation) ^^ {
    case left~None => left
    case left~Some(_~right) => left ∧ right
  }
  def negation: Parser[Sentence] = opt("~")~pSymbol ^^ {
    case None~term => term
    case Some(_)~term => ¬(term)
  }

  def pSymbol: Parser[Sentence] = ident ^^ {
    case x if x == "True" => AtomicSentence(True)
    case x if x == "False" => AtomicSentence(False)
    case x => AtomicSentence(PSymbol(x))
  } | "("~>sentence<~")" | "["~>sentence<~"]" | sentence

  def parse(in: String) = parseAll(sentence,in).get
}