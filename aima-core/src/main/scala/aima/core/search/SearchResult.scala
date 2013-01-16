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

package aima.core.search

/**
 * Represents the result of any search. Successful searches will return Success(result) while unsuccessful will be
 * either due to a Failure, typically because no solution exists, or a Cutoff, because a solution may exist,
 * but the state space that was explored did not result in a solution.
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/21/12
 */
sealed abstract class SearchResult[+A] {self =>

  /**
   * Returns true if the search result is a failure or a cutoff, true otherwise
   */
  def isEmpty: Boolean

  /**
   * Returns true if the search result is an instance of $solution, false otherwise.
   */
  def isDefined: Boolean = !isEmpty

  /** Returns the search result's value.
    * @note The search result must be nonEmpty.
    * @throws Predef.NoSuchElementException if the search result is empty.
    */
  def get: A

  /** Returns false if the search result is $none, true otherwise.
    * @note   Implemented here to avoid the implicit conversion to Iterable.
    */
  final def nonEmpty: Boolean = isDefined

  /** Returns true if this search result is nonempty '''and''' the predicate
    * $p returns true when applied to this $search result's value.
    * Otherwise, returns false.
    *
    * @param  p   the predicate to test
    */
  @inline final def exists(p: A => Boolean): Boolean = !isEmpty && p(this.get)

  /** Returns true if this search result is empty '''or''' the predicate
    * $p returns true when applied to this $search result's value.
    *
    * @param  p   the predicate to test
    */
  @inline final def forall(p: A => Boolean): Boolean = isEmpty || p(this.get)

  /** Apply the given procedure $f to the search result's value,
    * if it is nonempty. Otherwise, do nothing.
    *
    * @param  f   the procedure to apply.
    * @see map
    * @see flatMap
    */
  @inline final def foreach[U](f: A => U) {
    if (!isEmpty) f(this.get)
  }

  /** Returns this $search result if it is nonempty,
    * otherwise return the result of evaluating `alternative`.
    * @param alternative the alternative expression.
    */
  @inline final def orElse[B >: A](alternative: => SearchResult[B]): SearchResult[B] =
    if (isEmpty) alternative else this

  /** Returns a singleton iterator returning the $search result's value
    * if it is nonempty, or an empty iterator if the search result is empty.
    */
  def iterator: Iterator[A] = if (isEmpty) collection.Iterator.empty else collection.Iterator.single(this.get)

  /** Returns a singleton list containing the $search result's value
    * if it is nonempty, or the empty list if the $search result is empty.
    */
  def toList: List[A] = if (isEmpty) List() else new ::(this.get, Nil)

  def toOption: Option[A] = if (isEmpty) None else Some(this.get)

  /** Returns a [[scala.util.Left]] containing the given
    * argument `left` if this $search result is empty, or
    * a [[scala.util.Right]] containing this $search result's value if
    * this is nonempty.
    *
    * @param left the expression to evaluate and return if this is empty
    * @see toLeft
    */
  @inline final def toRight[X](left: => X): Either[X, A] = if (isEmpty) Left(left) else Right(this.get)

  /** Returns a [[scala.util.Right]] containing the given
    * argument `right` if this is empty, or
    * a [[scala.util.Left]] containing this $search result's value
    * if this $search result is nonempty.
    *
    * @param right the expression to evaluate and return if this is empty
    * @see toRight
    */
  @inline final def toLeft[X](right: => X): Either[A, X] = if (isEmpty) Right(right) else Left(this.get)
}

case class Success[+A](result: A) extends SearchResult[A] {
  def isEmpty: Boolean = false
  def get: A = result
}

sealed abstract class EmptySearchResult extends SearchResult[Nothing] {
  def isEmpty: Boolean = true
  def get: Nothing = throw new NoSuchElementException("None.get")
}
case object Failure extends EmptySearchResult
case object Cutoff extends EmptySearchResult
