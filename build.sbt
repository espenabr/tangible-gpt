ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "3.3.0"

val http4sVersion = "1.0.0-M40"
val circeVersion  = "0.14.5"

lazy val root = (project in file(".")).settings(
  name := "gpt-chat-semiautomation",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-effect"         % "3.5.1",
    "org.typelevel" %% "cats-effect-kernel"  % "3.5.1",
    "org.typelevel" %% "cats-effect-std"     % "3.5.1",
    "org.http4s"    %% "http4s-ember-client" % http4sVersion,
    "org.http4s"    %% "http4s-ember-server" % http4sVersion,
    "org.http4s"    %% "http4s-dsl"          % http4sVersion,
    "org.http4s"    %% "http4s-client"       % http4sVersion,
    "org.typelevel" %% "log4cats-slf4j"      % "2.6.0",
    "io.circe"      %% "circe-core"          % circeVersion,
    "io.circe"      %% "circe-generic"       % circeVersion,
    "io.circe"      %% "circe-parser"        % circeVersion,
    "org.http4s"    %% "http4s-circe"        % http4sVersion
  )
)
