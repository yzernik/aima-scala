package aima.core.logic.propositional

import org.scalatest.{FeatureSpec, GivenWhenThen}
import aima.core.logic.propositional.PropositionalLogicParser._
import aima.core.logic.propositional.grammar._
import org.scalatest.matchers.ShouldMatchers

class PropositionalParserTest extends FeatureSpec with GivenWhenThen with ShouldMatchers {
  feature("Can parse sentences with 1 or fewer operators") {
    scenario("A") {
      Given("the string \"A\"")
      val string = "A"
      When("it is parsed")
      val result = parseAll(sentence, string).get
      Then("AtomicSentence(PSymbol(A)) is the result")
      result should equal (AtomicSentence(PSymbol("A")))
    }

    scenario("¬(A)") {
      Given("the string \"~A\"")
      val string = "~A"
      When("it is parsed")
      val result = parseAll(sentence, string).get
      Then("¬(AtomicSentence(PSymbol(A))) is the result")
      result should equal (¬(AtomicSentence(PSymbol("A"))))
    }

    scenario("A ∨ B") {
      Given("the string \"A | B\"")
      val string = "A | B"
      When("it is parsed")
      val result = parseAll(sentence, string).get
      Then("AtomicSentence(PSymbol(A)) ∨ AtomicSentence(PSymbol(B)) is the result")
      result should equal (AtomicSentence(PSymbol("A")) ∨ AtomicSentence(PSymbol("B")))
    }

    scenario("A ∧ B") {
      Given("the string \"A & B\"")
      val string = "A & B"
      When("it is parsed")
      val result = parseAll(sentence, string).get
      Then("AtomicSentence(PSymbol(A)) ∧ AtomicSentence(PSymbol(B)) is the result")
      result should equal (AtomicSentence(PSymbol("A")) ∧ AtomicSentence(PSymbol("B")))
    }

    scenario ("A ⇾ B") {
      Given("the string \"A => B\"")
      val string = "A => B"
      When("it is parsed")
      val result = parseAll(sentence, string).get
      Then("AtomicSentence(PSymbol(A)) ⇾ AtomicSentence(PSymbol(B)) is the result")
      result should equal(AtomicSentence(PSymbol("A")) ⇾ AtomicSentence(PSymbol("B")))
    }

    scenario ("A ⇔ B") {
      Given("the string \"A <=> B\"")
      val string = "A <=> B"
      When("it is parsed")
      val result = parseAll(sentence, string).get
      Then("AtomicSentence(PSymbol(A)) ⇔ AtomicSentence(PSymbol(B)) is the result")
      result should equal(AtomicSentence(PSymbol("A")) ⇔ AtomicSentence(PSymbol("B")))
    }
  }

  feature("Can parse sentences with 2 operators") {
    scenario("A ∧ ¬(B)") {
      Given("the string \"A & ~B)\"")
      val string = "A & ~B"
      When("it is parsed")
      val result = parseAll(sentence, string).get
      Then("AtomicSentence(PSymbol(A)) ∧ ¬(AtomicSentence(PSymbol(B))) is the result")
      result should equal(AtomicSentence(PSymbol("A")) ∧ ¬(AtomicSentence(PSymbol("B"))))
    }
    scenario("A ∧ B ∨ C") {
      Given("the string \"A & B | C\"")
      val string = "A & B | C"
      When("it is parsed")
      val result = parseAll(sentence, string).get
      Then("(AtomicSentence(PSymbol(A)) ∧ AtomicSentence(PSymbol(B))) ∨ AtomicSentence(PSymbol(C)) is the result")
      result should equal((AtomicSentence(PSymbol("A")) ∧ AtomicSentence(PSymbol("B"))) ∨ AtomicSentence(PSymbol("C")))
    }

    scenario("A ⇾ B ∨ C") {
      Given("the string \"A => B | C\"")
      val string = "A => B | C"
      When("it is parsed")
      val result = parseAll(sentence, string).get
      Then("AtomicSentence(PSymbol(A)) ⇾ (AtomicSentence(PSymbol(B)) ∨ AtomicSentence(PSymbol(C))) is the result")
      result should equal(AtomicSentence(PSymbol("A")) ⇾ (AtomicSentence(PSymbol("B")) ∨ AtomicSentence(PSymbol("C"))))
    }

    scenario("A ∧ B ⇾ C") {
      Given("the string \"A & B => C\"")
      val string = "A & B => C"
      When("it is parsed")
      val result = parseAll(sentence, string).get
      Then("(AtomicSentence(PSymbol(A)) ∧ AtomicSentence(PSymbol(B))) ⇾ AtomicSentence(PSymbol(C)) is the result")
      result should equal((AtomicSentence(PSymbol("A")) ∧ AtomicSentence(PSymbol("B"))) ⇾ AtomicSentence(PSymbol("C")))
    }
  }

  feature("Can parse sentences with parentheses") {
    scenario("A ∧ (B ∨ C)") {
      Given("the string \"A & (B | C)\"")
      val string = "A & (B | C)"
      When("it is parsed")
      val result = parseAll(sentence, string).get
      Then("AtomicSentence(PSymbol(A)) ∧ (AtomicSentence(PSymbol(B)) ∨ AtomicSentence(PSymbol(C))) is the result")
      result should equal(AtomicSentence(PSymbol("A")) ∧ (AtomicSentence(PSymbol("B")) ∨ AtomicSentence(PSymbol("C"))))
    }

    scenario("(A ⇾ B) ∨ C") {
      Given("the string \"(A => B) | C\"")
      val string = "(A => B) | C"
      When("it is parsed")
      val result = parseAll(sentence, string).get
      Then("(AtomicSentence(PSymbol(A)) ⇾ AtomicSentence(PSymbol(B))) ∨ AtomicSentence(PSymbol(C)) is the result")
      result should equal((AtomicSentence(PSymbol("A")) ⇾ AtomicSentence(PSymbol("B"))) ∨ AtomicSentence(PSymbol("C")))
    }

    scenario("A ∧ (B ⇾ C)") {
      Given("the string \"A & (B => C)\"")
      val string = "A & (B => C)"
      When("it is parsed")
      val result = parseAll(sentence, string).get
      Then("AtomicSentence(PSymbol(A)) ∧ (AtomicSentence(PSymbol(B)) ⇾ AtomicSentence(PSymbol(C))) is the result")
      result should equal(AtomicSentence(PSymbol("A")) ∧ (AtomicSentence(PSymbol("B")) ⇾ AtomicSentence(PSymbol("C"))))
    }
  }
}