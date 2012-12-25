package aima.core.logic.fol

import java.util.concurrent.atomic.AtomicLong

trait FOLDefaults {
  implicit val defaultStandardVariable = new DefaultStandardVariable()
  final class DefaultStandardVariable extends StandardVariable {
    private val number = new AtomicLong()
    def apply(variable: Variable): String = variable.symbol + "$$" + number.incrementAndGet() + "$"
  }
  implicit val defaultStandardFunction = new DefaultStandardFunction()
  final class DefaultStandardFunction extends StandardFunction{
    private val number = new AtomicLong()
    def apply(variables: Set[Term]): AFunction = {
      val name = "F$" + number.incrementAndGet() + "$$"
      if (variables.isEmpty) ConstantFunction(name) else Function(name, variables)
    }
  }
}
