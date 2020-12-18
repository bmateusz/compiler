name := "compiler"

version := "0.1"

scalaVersion := "2.13.4"

// scalacOptions := Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.3" % "test",
  "org.scalacheck" %% "scalacheck" % "1.15.2" % "test",
  "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % "test"
)

logBuffered in Test := false
