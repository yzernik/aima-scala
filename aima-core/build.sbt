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
name := "aima-core"

version := "0.1"

scalaVersion := "2.10.0"

scalacOptions ++= Seq("-optimise", "-deprecation", "-unchecked")

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "3.0.1" % "test"