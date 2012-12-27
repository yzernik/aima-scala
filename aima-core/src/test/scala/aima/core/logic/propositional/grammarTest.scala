package aima.core.logic.propositional

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{GivenWhenThen, FeatureSpec}
import aima.core.logic.propositional.grammar._
import aima.core.logic.propositional.Connective._

object grammarTest extends FeatureSpec with GivenWhenThen with ShouldMatchers with GeneratorDrivenPropertyChecks {
  val maxComplexSize: Int = 10

  def complexSentences(left: Sentence, right: Sentence): List[ComplexSentence] =
    List(¬(left), left ∨ right, left ∧ right, left ⇾ right, left ⇔ right)

  val gen1PropositionSymbol: Gen[PropositionSymbol] = for {
    symbol <- alphaUpperChar
  } yield PSymbol(symbol.toString)

  val gen1Model: Gen[Model] = for {
    pSymbol <- gen1PropositionSymbol
    truth <- arbitrary[Boolean]
  } yield Map(pSymbol → truth)

  val gen1Not1Level: Gen[¬] = for {
    pSymbol <- gen1PropositionSymbol
  } yield ¬(pSymbol)

  val gen1Or1Level: Gen[∨] = for {
    left <- gen1PropositionSymbol
    right <- gen1PropositionSymbol
  } yield left ∨ right

  val gen1And1Level: Gen[∧] = for {
    left <- gen1PropositionSymbol
    right <- gen1PropositionSymbol
  } yield left ∧ right

  val gen1Implies1Level: Gen[⇾] = for {
    left <- gen1PropositionSymbol
    right <- gen1PropositionSymbol
  } yield left ⇾ right

  val gen1Iff1Level: Gen[⇔] = for {
    left <- gen1PropositionSymbol
    right <- gen1PropositionSymbol
  } yield left ⇔ right

  val gen5Symbol: Gen[String] = for {
    a <- alphaUpperChar
    b <- listOfN(scala.util.Random.nextInt(6), alphaNumChar)
  } yield (a :: b).mkString

  val gen5PropositionSymbol: Gen[PropositionSymbol] = for {
    symbol <- gen5Symbol
  } yield if (symbol == "True") True else if (symbol == "False") False else PSymbol(symbol)

  val gen5Model: Gen[Model] = for {
    pSymbol <- gen5PropositionSymbol
    truth <- arbitrary[Boolean]
  } yield Map(pSymbol → truth)

  def gen5Complex(size: Int): Gen[ComplexSentence] = for {
    left <- gen5Sentence(size + 1)
    right <- gen5Sentence(size + 1)
    complex <- oneOf(complexSentences(left, right))
  } yield complex

  def gen5Sentence(size: Int): Gen[Sentence] =
    if (size < maxComplexSize) frequency((3, gen5Complex(size)), (1, gen5PropositionSymbol)) else gen5PropositionSymbol

  val gen5Not: Gen[¬] = for {
    sentence <- gen5Sentence(0)
  } yield ¬(sentence)

  val get5Or: Gen[∨] = for {
    left <- gen5Sentence(0)
    right <- gen5Sentence(0)
  } yield left ∨ right

  val gen5And: Gen[∧] = for {
    left <- gen5Sentence(0)
    right <- gen5Sentence(0)
  } yield left ∧ right

  val gen5Implies: Gen[⇾] = for {
    premise <- gen5Sentence(0)
    conclusion <- gen5Sentence(0)
  } yield premise ⇾ conclusion

  val gen5Iff: Gen[⇔] = for {
    left <- gen5Sentence(0)
    right <- gen5Sentence(0)
  } yield left ⇔ right

  feature("Proposition symbols") {
    info("Proposition symbols are necessary for propositional logic")
    val pattern = """[A-Z][a-z0-9A-Z]*""".r.pattern
    scenario("Proposition symbols should only be allowed for strings that match [A-Z][a-z0-9A-Z]*") {
      forAll("symbol", minSuccessful(500)) {(symbol: String) =>
        if (pattern.matcher(symbol).matches()) {
          PSymbol(symbol).symbol should equal(symbol)
        } else {
          intercept[IllegalArgumentException] {
            PSymbol(symbol)
          }
        }
      }
    }
    scenario("Randomly generated symbols that match [A-Z][a-z0-9A-Z]* should create valid PSymbol objects") {
      val genSymbol = for {
        a <- alphaUpperChar
        b <- listOf(alphaNumChar)
      } yield (a :: b).mkString

      forAll((genSymbol, "symbol")) {symbol =>
        PSymbol(symbol).symbol should equal(symbol)
      }
    }
    scenario("True is true in every model and false in no models") {
      forAll((gen5Model, "model")) {model =>
        True isTrueIn model should be(true)
        True isFalseIn model should be(false)
      }
    }
    scenario("False is false in every model and true in no models") {
      forAll((gen5Model, "model")) {model =>
        False isTrueIn model should be(false)
        False isFalseIn model should be(true)
      }
    }
    scenario("PropositionSymbol#toString returns the symbol") {
      forAll((gen5PropositionSymbol, "pSymbol")) {pSymbol =>
        pSymbol.toString should equal(pSymbol.symbol)
      }
    }
  }
  feature("¬ complex sentences") {
    scenario("¬(sentence) reverses isTrueIn and isFalseIn model for sentence") {
      forAll((gen1Model, "model"), (gen1Not1Level, "¬")) {(model, not) =>
        if (not.sentence isTrueIn model) {
          not isFalseIn model should be(true)
        } else {
          not isFalseIn model should be(false)
        }
        if (not.sentence isFalseIn model)
          not isTrueIn model should be(true)
        else
          not isTrueIn model should be(false)
      }
    }
    scenario("¬#toString returns ~sentence") {

    }
  }
}
