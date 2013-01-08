package aima.core.probability

import aima.core.probability.impl.{FiniteRandomVariable, FiniteSingleWorld}

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): page 524.<br>
 * <br>
 * Each factor is a matrix indexed by its argument variables.
 *
 * @author Alex DiCarlo
 */
trait Factor extends Traversable[(FiniteSingleWorld, Double)] {
  /**
   * A consistent ordered Set of the argument variables for this Factor.
   * Note, this is represented as a List where variables.distinct == variables is true
   */
  def variables: List[FiniteRandomVariable[_]]
  /**
   * The List[Double] used to represent the Factor.
   */
  def values: List[Double]

  /**
   * Sum out the provided variables from this Factor creating a new Factor of
   * the remaining variables with their values updated with the summed out
   * random variables.<br>
   * <br>
   * see: AIMA3e page 527.<br>
   * <br>
   *
   * @param variables the random variables to sum out.
   * @return a new Factor containing any remaining random variables not summed
   *         out and a new set of values updated with the summed out values.
   */
  def sumOut(variables: Set[FiniteRandomVariable[_]]): Factor

  /**
   * Pointwise multiplication of this Factor by a given multiplier, creating a
   * new Factor representing the product of the two.<br>
   * <br>
   * see: AIMA3e Figure 14.10 page 527.<br>
   * <br>
   * Note: Default Factor multiplication is not commutative. The reason is
   * because the order of the variables comprising a Factor dictate the
   * ordering of the values for that factor. The default order of the
   * variables of the Factor returned is the order of the variables as they
   * are seen, as read from the left to right term, for e.g.: <br>
   * <br>
   * f<sub>1</sub>(Y)f<sub>2</sub>(X, Y)<br>
   * <br>
   * would give a Factor of the following form: <br>
   * Y, X<br>
   * <br>
   * i.e. an ordered union of the variables from the two factors. <br>
   * To override the default order of the product use pointwiseProductPOS().
   *
   * @param multiplier
   *
   * @return a new Factor representing the pointwise product of this and the
   *         passed in multiplier. The order of the variables comprising the
   *         product factor is the ordered union of the left term (this) and
   *         the right term (multiplier).
   *
   * @see Factor#pointwiseProductPOS(Factor, RandomVariable...)
   */
  def pointwiseProduct(multiplier: Factor): Factor =
    pointwiseProductPOS(multiplier, (variables ::: multiplier.variables).distinct)

  /**
   * Pointwise Multiplication - Product Order Specified (POS).<br>
   * <br>
   * see: AIMA3e Figure 14.10 page 527.<br>
   * <br>
   * Pointwise multiplication of this Factor by a given multiplier, creating a
   * new Factor representing the product of the two. The order of the
   * variables comprising the product will match those specified.
   *
   * @param multiplier
   * @param prodVarOrder
         *            the order the variables comprising the product are to be in.
   *
   * @return a new Factor representing the pointwise product of this and the
   *         passed in multiplier. The order of the variables comprising the
   *         product distribution is the order specified.
   *
   * @see Factor#pointwiseProduct(Factor)
   */
  def pointwiseProductPOS(multiplier: Factor, prodVarOrder: List[FiniteRandomVariable[_]]): Factor
}
