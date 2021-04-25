ThisBuild / scalaVersion := "2.13.5"

lazy val root = project.in(file(".")).
  aggregate(compiler.js, compiler.jvm).
  settings(
    publish := {},
    publishLocal := {},
  )

lazy val compiler = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .in(file("."))
  .settings(
    name := "compiler",
    version := "0.1-SNAPSHOT",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.8" % "test",
      "org.scalacheck" %% "scalacheck" % "1.15.3" % "test",
      "org.scalatestplus" %% "scalacheck-1-15" % "3.2.8.0" % "test"
    )
  )
  .jvmSettings(
    // Add JVM-specific settings here
  )
  .jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0"
    // Add JS-specific settings here
  )
  .nativeSettings(
    // Add Native-specific settings here
  )
