ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "InterpolationServer"
  )

val Http4sVersion = "1.0.0-M38"
val CirceVersion = "0.14.5"
libraryDependencies ++= Seq(
  "org.http4s"      %% "http4s-blaze-server" % Http4sVersion,
  "org.http4s"      %% "http4s-circe"        % Http4sVersion,
  "org.http4s"      %% "http4s-dsl"          % Http4sVersion,
  "io.circe"        %% "circe-generic"       % CirceVersion,
)

libraryDependencies += "org.apache.poi" % "poi-ooxml" % "5.2.3"

libraryDependencies += "org.slf4j" % "slf4j-api" % "2.0.7"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.4.7" % Runtime




