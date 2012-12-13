package aima.core.probability

import scala.collection.immutable.MapProxy

case class World(assignments: Map[Int, Assignment[_]]) extends MapProxy[Int, Assignment[_]] {
  require(
    assignments forall {pair => pair._1 == pair._2.variable.num},
    "Assignments do not have correct map! Some key != value.variable.num")
  def self = assignments
}