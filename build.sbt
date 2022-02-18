name := "taklib"

lazy val commonSettings = Seq(
  version := "0.2.0",
  scalaVersion := "2.13.8"
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
scalacOptions in (taklib, Compile, console) ~= (_.filterNot(
  Set("-Xfatal-warnings", "-Ywarn-unused-import").contains
))
scalacOptions in (takcli, Compile, console) ~= (_.filterNot(
  Set("-Xfatal-warnings", "-Ywarn-unused-import").contains
))
scalacOptions in (tpsserver, Compile, console) ~= (_.filterNot(
  Set("-Xfatal-warnings", "-Ywarn-unused-import").contains
))
scalacOptions in (opentak, Compile, console) ~= (_.filterNot(
  Set("-Xfatal-warnings", "-Ywarn-unused-import").contains
))

resolvers += Resolver.sonatypeRepo("releases")

val catsVersion = "2.7.0"
val parserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0"
val dependencies = Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  parserCombinators,
  "org.scala-graph" %% "graph-core" % "1.13.4"
)
val testDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.2.11" % "test",
  "org.scalacheck" %% "scalacheck" % "1.15.4" % "test",
  "com.ironcorelabs" %% "cats-scalatest" % "3.1.1" % "test",
  "org.typelevel" %% "discipline-core" % "1.4.0" % "test",
  "org.typelevel" %% "discipline-scalatest" % "2.0.0",
  "org.typelevel" %% "cats-kernel-laws" % catsVersion % "test",
  "org.scalatestplus" %% "scalacheck-1-15" % "3.2.11.0" % "test"
)

taklib / libraryDependencies ++= dependencies
taklib / libraryDependencies ++= testDependencies
takcli / libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "3.3.5"
)

resolvers in tpsserver += Resolver.sonatypeRepo("snapshots")
val http4sVersion = "0.23.10"
val circeVersion = "0.14.1"
tpsserver / libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "io.circe" %% "circe-optics" % circeVersion % "test",
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion % "test",
  "org.http4s" %% "http4s-client" % http4sVersion % "test",
  "ch.qos.logback" % "logback-classic" % "1.2.1"
) ++ testDependencies

opentak / libraryDependencies += parserCombinators
opentak / libraryDependencies ++= testDependencies

initialCommands in (taklib, console) += "import com.github.daenyth.taklib._"

taklib / coverageEnabled := true
