package aima.core.logic.propositional

import aima.core.logic.propositional.PropositionalLogicParser._
import aima.core.logic.propositional.grammar._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FeatureSpec, GivenWhenThen}

class PropositionalParserTest
  extends FeatureSpec with GivenWhenThen with ShouldMatchers with GeneratorDrivenPropertyChecks {
  feature("Can parse sentences with 1 or fewer operators") {
    scenario("A") {
      Given("the string \"A\"")
      val string = "A"
      When("it is parsed")
      val result = parse(string)
      Then("AtomicSentence(PSymbol(A)) is the result")
      result should equal(AtomicSentence(PSymbol("A")))
    }

    scenario("¬(A)") {
      Given("the string \"~A\"")
      val string = "~A"
      When("it is parsed")
      val result = parse(string)
      Then("¬(AtomicSentence(PSymbol(A))) is the result")
      result should equal(¬(AtomicSentence(PSymbol("A"))))
    }

    scenario("A ∨ B") {
      Given("the string \"A | B\"")
      val string = "A | B"
      When("it is parsed")
      val result = parse(string)
      Then("AtomicSentence(PSymbol(A)) ∨ AtomicSentence(PSymbol(B)) is the result")
      result should equal(AtomicSentence(PSymbol("A")) ∨ AtomicSentence(PSymbol("B")))
    }

    scenario("A ∧ B") {
      Given("the string \"A & B\"")
      val string = "A & B"
      When("it is parsed")
      val result = parse(string)
      Then("AtomicSentence(PSymbol(A)) ∧ AtomicSentence(PSymbol(B)) is the result")
      result should equal(AtomicSentence(PSymbol("A")) ∧ AtomicSentence(PSymbol("B")))
    }

    scenario("A ⇾ B") {
      Given("the string \"A => B\"")
      val string = "A => B"
      When("it is parsed")
      val result = parse(string)
      Then("AtomicSentence(PSymbol(A)) ⇾ AtomicSentence(PSymbol(B)) is the result")
      result should equal(AtomicSentence(PSymbol("A")) ⇾ AtomicSentence(PSymbol("B")))
    }

    scenario("A ⇔ B") {
      Given("the string \"A <=> B\"")
      val string = "A <=> B"
      When("it is parsed")
      val result = parse(string)
      Then("AtomicSentence(PSymbol(A)) ⇔ AtomicSentence(PSymbol(B)) is the result")
      result should equal(AtomicSentence(PSymbol("A")) ⇔ AtomicSentence(PSymbol("B")))
    }
  }

  feature("Can parse sentences with 2 operators") {
    scenario("A ∧ ¬(B)") {
      Given("the string \"A & ~B)\"")
      val string = "A & ~B"
      When("it is parsed")
      val result = parse(string)
      Then("AtomicSentence(PSymbol(A)) ∧ ¬(AtomicSentence(PSymbol(B))) is the result")
      result should equal(AtomicSentence(PSymbol("A")) ∧ ¬(AtomicSentence(PSymbol("B"))))
    }
    scenario("A ∧ B ∨ C") {
      Given("the string \"A & B | C\"")
      val string = "A & B | C"
      When("it is parsed")
      val result = parse(string)
      Then("(AtomicSentence(PSymbol(A)) ∧ AtomicSentence(PSymbol(B))) ∨ AtomicSentence(PSymbol(C)) is the result")
      result should equal((AtomicSentence(PSymbol("A")) ∧ AtomicSentence(PSymbol("B"))) ∨ AtomicSentence(PSymbol("C")))
    }

    scenario("A ⇾ B ∨ C") {
      Given("the string \"A => B | C\"")
      val string = "A => B | C"
      When("it is parsed")
      val result = parse(string)
      Then("AtomicSentence(PSymbol(A)) ⇾ (AtomicSentence(PSymbol(B)) ∨ AtomicSentence(PSymbol(C))) is the result")
      result should equal(AtomicSentence(PSymbol("A")) ⇾ (AtomicSentence(PSymbol("B")) ∨ AtomicSentence(PSymbol("C"))))
    }

    scenario("A ∧ B ⇾ C") {
      Given("the string \"A & B => C\"")
      val string = "A & B => C"
      When("it is parsed")
      val result = parse(string)
      Then("(AtomicSentence(PSymbol(A)) ∧ AtomicSentence(PSymbol(B))) ⇾ AtomicSentence(PSymbol(C)) is the result")
      result should equal((AtomicSentence(PSymbol("A")) ∧ AtomicSentence(PSymbol("B"))) ⇾ AtomicSentence(PSymbol("C")))
    }
  }

  feature("Can parse sentences with parentheses") {
    scenario("A ∧ (B ∨ C)") {
      Given("the string \"A & (B | C)\"")
      val string = "A & (B | C)"
      When("it is parsed")
      val result = parse(string)
      Then("AtomicSentence(PSymbol(A)) ∧ (AtomicSentence(PSymbol(B)) ∨ AtomicSentence(PSymbol(C))) is the result")
      result should equal(AtomicSentence(PSymbol("A")) ∧ (AtomicSentence(PSymbol("B")) ∨ AtomicSentence(PSymbol("C"))))
    }

    scenario("(A ⇾ B) ∨ C") {
      Given("the string \"(A => B) | C\"")
      val string = "(A => B) | C"
      When("it is parsed")
      val result = parse(string)
      Then("(AtomicSentence(PSymbol(A)) ⇾ AtomicSentence(PSymbol(B))) ∨ AtomicSentence(PSymbol(C)) is the result")
      result should equal((AtomicSentence(PSymbol("A")) ⇾ AtomicSentence(PSymbol("B"))) ∨ AtomicSentence(PSymbol("C")))
    }

    scenario("A ∧ (B ⇾ C)") {
      Given("the string \"A & (B => C)\"")
      val string = "A & (B => C)"
      When("it is parsed")
      val result = parse(string)
      Then("AtomicSentence(PSymbol(A)) ∧ (AtomicSentence(PSymbol(B)) ⇾ AtomicSentence(PSymbol(C))) is the result")
      result should equal(AtomicSentence(PSymbol("A")) ∧ (AtomicSentence(PSymbol("B")) ⇾ AtomicSentence(PSymbol("C"))))
    }
  }

  feature("Parser passes ScalaCheck") {
    scenario("ScalaCheck's randomly generated complex sentences of up to 2^10 symbols that are up to 6 characters "
      + "from [A-Z][a-z0-9A-Z]* are parsed correctly") {
      val genSymbol = for {
        a <- alphaUpperChar
        b <- listOfN(scala.util.Random.nextInt(6), alphaNumChar)
      } yield (a :: b).mkString

      val genAtomic: Gen[AtomicSentence] = for {
        symbol <- genSymbol
      } yield AtomicSentence(PSymbol(symbol))

      def complexSentences(left: Sentence, right: Sentence): List[ComplexSentence] =
        List(¬(left), left ∨ right, left ∧ right, left ⇾ right, left ⇔ right)

      def genComplex(size: Int): Gen[ComplexSentence] = for {
        left <- genSentence(size + 1)
        right <- genSentence(size + 1)
        complex <- oneOf(complexSentences(left, right))
      } yield complex

      def genSentence(size: Int): Gen[Sentence] =
        if (size < 10) frequency((3, genComplex(size)), (1, genAtomic)) else genAtomic

      forAll(genSentence(0)) {sentence =>
        val string = sentence.toString
        val result = parse(string)
        result should equal(sentence)
      }
    }
  }
}