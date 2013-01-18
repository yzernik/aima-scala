/*
 * Copyright 2012, 2013 Alex DiCarlo
 *
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

package aima.core.logic.fol

import java.util.concurrent.atomic.AtomicLong

trait FOLDefaults {
  implicit val defaultStandardVariable = new DefaultStandardVariable()
  final class DefaultStandardVariable extends StandardVariable {
    private val number = new AtomicLong()
    def apply(variable: Variable): String = variable.symbol + "$$" + number.incrementAndGet() + "$"
  }
  implicit val defaultStandardFunction = new DefaultStandardFunction()
  final class DefaultStandardFunction extends StandardFunction {
    private val number = new AtomicLong()
    def apply(variables: Set[Term]): AFunction = {
      val name = "F$" + number.incrementAndGet() + "$$"
      if (variables.isEmpty) ConstantFunction(name) else Function(name, variables)
    }
  }
}
