name := "taklib"

lazy val commonSettings = Seq(
  version := "0.1.0",
  scalaVersion := "2.12.1",
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8", // yes, this is 2 args
    "-feature",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xlint",
    "-Yno-adapted-args",
    // "-Ywarn-dead-code", // N.B. doesn't work well with the ??? hole
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Ywarn-unused",
    "-Ywarn-unused-import",
    "-Xfuture",
    "-Ypartial-unification"
  )
)

lazy val taklib = (project in file("taklib"))
  .settings(commonSettings, name := "taklib")
lazy val takcli = (project in file("takcli"))
  .dependsOn(taklib)
  .settings(commonSettings, name := "takcli")
// Remove these options in 'sbt console' because they're not nice for interactive usage
scalacOptions in (Compile, console) ~= (_.filterNot(Set("-Xfatal-warnings", "-Ywarn-unused-import").contains))

resolvers += Resolver.sonatypeRepo("releases")

val scalazVersion = "7.2.8"
val dependencies = Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
  "org.scala-graph" %% "graph-core" % "1.11.4",

  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.typelevel" %% "scalaz-scalatest" % "1.1.1" % "test",
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test"
)

libraryDependencies in taklib ++= dependencies
libraryDependencies in takcli ++= Seq(
  "org.scalaz" %% "scalaz-concurrent" % scalazVersion
)

initialCommands in console += "import com.github.daenyth.taklib._"
