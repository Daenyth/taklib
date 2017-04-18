name := "taklib"

lazy val commonSettings = Seq(
  version := "0.2.0",
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
lazy val tpsserver = (project in file("tpsserver"))
  .dependsOn(taklib)
  .settings(commonSettings, name := "tpsserver")
lazy val opentak = (project in file("opentak"))
  .dependsOn(taklib)
  .settings(commonSettings, name := "opentak")

// Remove these options in 'sbt console' because they're not nice for interactive usage
scalacOptions in (taklib, Compile, console) ~= (_.filterNot(Set("-Xfatal-warnings", "-Ywarn-unused-import").contains))
scalacOptions in (takcli, Compile, console) ~= (_.filterNot(Set("-Xfatal-warnings", "-Ywarn-unused-import").contains))
scalacOptions in (tpsserver, Compile, console) ~= (_.filterNot(Set("-Xfatal-warnings", "-Ywarn-unused-import").contains))
scalacOptions in (opentak, Compile, console) ~= (_.filterNot(Set("-Xfatal-warnings", "-Ywarn-unused-import").contains))

resolvers += Resolver.sonatypeRepo("releases")

val scalazVersion = "7.2.8"
val dependencies = Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
  "org.scala-graph" %% "graph-core" % "1.11.4"
)
val testDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.typelevel" %% "scalaz-scalatest" % "1.1.1" % "test",
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test"
)

libraryDependencies in taklib ++= dependencies
libraryDependencies in taklib ++= testDependencies
libraryDependencies in takcli ++= Seq(
  "org.scalaz" %% "scalaz-concurrent" % scalazVersion
)

resolvers in tpsserver += Resolver.sonatypeRepo("snapshots")
val http4sVersion = "0.17.0-M1"
val circeVersion = "0.7.0"
libraryDependencies in tpsserver ++= Seq(
  "io.circe" %% "circe-core"    % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser"  % circeVersion,
  "io.circe" %% "circe-optics" % circeVersion % "test",

  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-circe"        % http4sVersion,
  "org.http4s" %% "http4s-dsl"          % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion % "test",
  "org.http4s" %% "http4s-client"       % http4sVersion % "test",

  "ch.qos.logback" % "logback-classic" % "1.2.1"
) ++ testDependencies

initialCommands in (taklib, console) += "import com.github.daenyth.taklib._"

coverageEnabled in taklib := true
