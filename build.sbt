name := "taklib"

version := "1.0"

scalaVersion := "2.12.1"

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
// Remove these options in 'sbt console' because they're not nice for interactive usage
scalacOptions in (Compile, console) ~= (_.filterNot(Set("-Xfatal-warnings", "-Ywarn-unused-import").contains))

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.8",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",

  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.typelevel" %% "scalaz-scalatest" % "1.1.1" % "test",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "7.3.0-M9" % "test"
)
