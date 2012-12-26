import sbt._
import Keys._

object HelloBuild extends Build {
  lazy val root = Project(id = "aima-scala", base = file(".")) aggregate(aimaCore)

  lazy val aimaCore = Project(id = "aima-core", base = file("aima-core"))
}