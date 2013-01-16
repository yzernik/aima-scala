/*
 * This file is part of aima-scala.
 *
 * Aima-scala is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Aima-scala is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with aima-scala.  If not, see <http://www.gnu.org/licenses/>.
 */

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

  def pSymbol: Parser[Sentence] = """[A-Z][a-z0-9A-Z]*""".r ^^ {
    case x if x == "True" => True
    case x if x == "False" => False
    case x => PSymbol(x)
  } | "("~>sentence<~")" | sentence

  def parse(in: String): Sentence = parseAll(sentence,in).get
}