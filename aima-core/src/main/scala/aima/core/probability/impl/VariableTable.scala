package aima.core.probability.impl

import scala.annotation.tailrec
import aima.core.probability.impl.VariableTable._
import aima.core.util.MixedRadixNumber.mixedRadixValue

private[probability] case class VariableTable(variables: List[FiniteRandomVariable[_]]) {
  val radices: List[Int] = createRadices(variables)
  val varInfos: VarInfos = createVarInfos(variables)
  val size: Int = expectedSizeOfTable(variables)

  def index(assignments: List[AssignmentProposition]): Int = {
    require(assignments.size == varInfos.size,
      "Assignments passed in is not the same size as variables making up probability table.")
    @tailrec
    def recur(assignments: List[SingleAssignment[_]], radixValues: List[Int]): List[Int] = assignments match {
      case SingleAssignment(variable@FiniteRandomVariable(_, _), value) :: tail => varInfos get variable match {
        case Some(rvInfo) =>
          recur(tail, radixValues.updated(rvInfo.radixIndex, rvInfo.domain.indexedValues indexOf value))
        case None => throw new IllegalArgumentException(
          s"Assignment passed for a variable that is not part of this probability table:$variable")
      }
      case Nil => radixValues
    }
    val radixValues = recur((assignments map {_.assign}).toList, List.fill(assignments.size)(0))
    mixedRadixValue(radixValues, radices).toInt
  }
}

private[probability] object VariableTable {
  def expectedSizeOfTable(variables: List[FiniteRandomVariable[_]]): Int = (variables foldLeft 1)
  {case (expectedSize, FiniteRandomVariable(_, domain)) => expectedSize * domain.finiteValues.size}

  def createRadices(variables: List[FiniteRandomVariable[_]]): List[Int] = {
    for {radixIdx <- (0 until variables.size).toList
      variable = variables(radixIdx)
      radix = variable.domain.finiteValues.size
    } yield radix
  }

  def createVarInfos(variables: List[FiniteRandomVariable[_]]): VarInfos = {
    val entries = for {radixIdx <- 0 until variables.size
      variable = variables(radixIdx)
    } yield (variable → RVInfo(variable, radixIdx))
    VarInfos(entries.toMap)
  }

  case class RVInfo[A](variable: FiniteRandomVariable[A], radixIndex: Int) {
    val domain: FiniteDomain[A] = variable.domain
    def indexFor(value: A): Int = domain.indexedValues.indexOf(value)
    def valueAt(index: Int): A = domain.indexedValues(index)
    def createAssignment(index: Int): SingleAssignment[A] = SingleAssignment(variable, valueAt(index))
  }

  case class VarInfos(rvInfoMap: Map[FiniteRandomVariable[_], RVInfo[_]]) {
    def size: Int = rvInfoMap.size
    def apply[A](variable: FiniteRandomVariable[A]): RVInfo[A] = rvInfoMap(variable).asInstanceOf[RVInfo[A]]
    def get[A](variable: FiniteRandomVariable[A]): Option[RVInfo[A]] =
      rvInfoMap.get(variable).asInstanceOf[Option[RVInfo[A]]]
  }
}
