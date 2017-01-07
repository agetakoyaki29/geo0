

lazy val root = (project in file("."))
  .settings(
    // organization := "com.github",
    name := "geo0",
    version := "0.0",
    scalaVersion := "2.11.8",

    libraryDependencies += scalactic,
    libraryDependencies += scalatest,

    logBuffered in Test := false
  )

// ---- lib ----
lazy val scalactic = "org.scalactic" %% "scalactic" % "2.2.5"
lazy val scalatest = "org.scalatest" %% "scalatest" % "2.2.5" % "test"
