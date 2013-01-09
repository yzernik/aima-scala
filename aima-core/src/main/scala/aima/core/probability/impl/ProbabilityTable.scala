package aima.core.probability.impl

import aima.core.probability._
import aima.core.probability.impl.VariableTable._
import aima.core.util.MixedRadixNumber
import aima.core.util.MixedRadixNumber.mixedRadixValue

private[probability] case class ProbabilityTable(values: List[Double], table: VariableTable)
  extends CategoricalDistribution with Factor {
  require(values.size == table.size, s"ProbabilityTable of length $values.size is not the correct size, should " +
    s"be $table.size in order to represent all possible combinations")
  private val radices = table.radices
  private val varInfos = table.varInfos
  private lazy val sum: Double = values.sum
  val variables: List[FiniteRandomVariable[_]] = table.variables
  override lazy val size: Int = values.length
  /**
   * Sequence of possible assignments the random variables in this table can take on with the associated probability
   */
  private lazy val worldProbabilities: IndexedSeq[(FiniteSingleWorld, Double)] =
    createProbabilities(values, radices, varInfos)

  def index(assignments: List[AssignmentProposition]): Int = table.index(assignments)

  def normalize(): CategoricalDistribution = ProbabilityTable(values map {_ / sum}, table)

  def marginal(variables: Set[FiniteRandomVariable[_]]): CategoricalDistribution = sumOutVariables(variables)

  def sumOut(variables: Set[FiniteRandomVariable[_]]): Factor = sumOutVariables(variables)

  def multiplyByPOS(
    multiplier: CategoricalDistribution,
    productVariableOrder: List[FiniteRandomVariable[_]]): CategoricalDistribution =
    product(multiplier.values, multiplier.variables, productVariableOrder)

  def pointwiseProductPOS(multiplier: Factor, productVariableOrder: List[FiniteRandomVariable[_]]): Factor =
    product(multiplier.values, multiplier.variables, productVariableOrder)

  def foreach[U](f: ((FiniteSingleWorld, Double)) => U) {
    worldProbabilities foreach {case (world, probability) => f(world, probability)}
  }

  private def sumOutVariables(vars: Set[FiniteRandomVariable[_]]): ProbabilityTable = {
    val remainingVariables = variables diff vars.toList
    val newTable = VariableTable(remainingVariables)
    val newValues = foldLeft(IndexedSeq.fill(expectedSizeOfTable(remainingVariables))(0.0)) {
      case (probabilities, (possibleWorld, probability)) =>
        val assignment = remainingVariables map {variable => AssignmentProposition(possibleWorld(variable))}
        val index = newTable.index(assignment)
        probabilities.updated(index, probabilities(index) + probability)
    }
    ProbabilityTable(newValues.toList, newTable)
  }

  // Divides probabilities by probability in divisor for corresponding assignments. If less assignments than
  // dividend, divides by divisor's probability for all possible combinations. (Similar to summing out)
  def divideBy(divisor: CategoricalDistribution): CategoricalDistribution = {
    require(divisor.variables forall variables.contains, "Divisor must be a subset of the dividend.")

    val quotientTable = table
    val diffVars = divisor.variables diff variables
    val diffRadices = createRadices(diffVars)
    val diffVarInfos = createVarInfos(diffVars)
    val quotientValues = (divisor foldLeft List.fill(expectedSizeOfTable(variables))(0.0)) {
      case (quotientValues, (divisorWorld@FiniteSingleWorld(assignments), probability)) =>
        // Sets radix values to value of divisorWorld assignment for that variable
        val quotientRadixValues = (assignments.keySet foldLeft List.fill(quotientTable.radices.length)(0)) {
          case (qRVs, variable) =>
            val varInfo = quotientTable.varInfos(variable)
            qRVs.updated(varInfo.radixIndex, varInfo.indexFor(divisorWorld(variable).value))
        }

        // We update the probability for all combinations of assignments for radices not assigned above.
        // If all were assigned (i.e. divisor.variables.length == variables.length), then the value is updated once
        val mixedRadixNumbers =
          (0 to MixedRadixNumber(0, diffRadices).maxAllowedValue.toInt) map {MixedRadixNumber(_, diffRadices)}
        (mixedRadixNumbers foldLeft quotientValues) {case (qValues, mixedRadixNumber) =>
          val mrnQuotientRadixValues = (diffVars foldLeft quotientRadixValues) {case (qRVs, variable) =>
            val diffRVInfo = diffVarInfos(variable)
            val quotientRVInfo = quotientTable.varInfos(variable)
            qRVs.updated(quotientRVInfo.radixIndex, mixedRadixNumber.numeralValue(diffRVInfo.radixIndex).toInt)
          }
          val index = mixedRadixValue(mrnQuotientRadixValues, quotientTable.radices).toInt
          val value = if (probability != 0) values(index) / probability else 0.0
          quotientValues.updated(index, value)
        }
    }
    ProbabilityTable(quotientValues, quotientTable)
  }

  private def product(
    multiplierValues: List[Double],
    multiplierVariables: List[FiniteRandomVariable[_]],
    productVariableOrder: List[FiniteRandomVariable[_]]): ProbabilityTable = {
    require(productVariableOrder == (variables union multiplierVariables),
      "Product variable order was not union of multipier and multiplicand variables")
    def termIdx(variables: List[FiniteRandomVariable[_]], possibleWorld: FiniteSingleWorld): Int = {
      val termValues = for (rv <- variables) yield rv -> possibleWorld(rv)
      indexOf(variables, FiniteSingleWorld(termValues.toMap))
    }

    val prodTable = VariableTable(productVariableOrder)
    val prodValues = for {possibleWorld <- createPossibleWorlds(prodTable.radices, prodTable.varInfos)
      multiplicandIdx = termIdx(variables, possibleWorld)
      multiplierIdx = termIdx(multiplierVariables, possibleWorld)
    } yield values(multiplicandIdx) * multiplierValues(multiplierIdx)
    ProbabilityTable(prodValues.to[List], prodTable)
  }

  private def createProbabilities(
    values: List[Double],
    radices: List[Int],
    varInfos: VarInfos): IndexedSeq[(FiniteSingleWorld, Double)] = {
    for {i <- 0 to MixedRadixNumber(0, radices).maxAllowedValue.toInt
      mixedRadixNumber = MixedRadixNumber(i, radices)
      possibleWorld = generateWorld(mixedRadixNumber, varInfos)
      probability = values(mixedRadixNumber.value.toInt)
    } yield (possibleWorld → probability)
  }

  private def createPossibleWorlds(radices: List[Int], varInfos: VarInfos): IndexedSeq[FiniteSingleWorld] = {
    for {i <- 0 to MixedRadixNumber(0, radices).maxAllowedValue.toInt
      mixedRadixNumber = MixedRadixNumber(i, radices)
      possibleWorld = generateWorld(mixedRadixNumber, varInfos)
    } yield possibleWorld
  }

  private def generateWorld(mrn: MixedRadixNumber, varInfos: VarInfos): FiniteSingleWorld = {
    val map = varInfos.rvInfoMap.values map {rvInfo =>
      rvInfo.variable → rvInfo.createAssignment(mrn.numeralValue(rvInfo.radixIndex).toInt)
    }
    FiniteSingleWorld(map.toMap)
  }

  private def indexOf(variables: List[FiniteRandomVariable[_]], world: FiniteSingleWorld): Int = {
    val mrv = for {(idx, radixIdx) <- (0 until variables.length) zip (variables.length - 1 to 0)
      finiteDomain = variables(idx).domain
      radixValue = finiteDomain.indexedValues.indexOf(world(variables(idx)))
      radice = finiteDomain.indexedValues.length
    } yield (radixValue, radice)
    val (radixValues, radices) = mrv.unzip
    mixedRadixValue(radixValues.toList, radices.toList).toInt
  }
}