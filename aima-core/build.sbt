name := "aima-core"

version := "0.1"

scalaVersion := "2.10.0-RC5"

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"