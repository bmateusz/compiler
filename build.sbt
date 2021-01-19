logBuffered in Test := false

ThisBuild / scalaVersion := "2.13.4"

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
      "org.scalatest" %% "scalatest" % "3.2.3" % "test",
      "org.scalacheck" %% "scalacheck" % "1.15.2" % "test",
      "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % "test"
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
