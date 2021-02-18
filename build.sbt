ThisBuild / version := "0.1"

ThisBuild / scalaVersion := "2.11.12"

ThisBuild / javacOptions :=
  Seq("-source", "1.8", "-target", "1.8", "-encoding", "utf-8", "-Xlint:unchecked")

ThisBuild / resolvers += "Tencent Public" at "https://mirrors.tencent.com/nexus/repository/maven-public/"

lazy val scalatest = "org.scalatest" %% "scalatest" % "3.1.2" % Test
lazy val slf4j12 = "org.slf4j" % "slf4j-log4j12" % "1.6.1"
lazy val log4j = "log4j" % "log4j" % "1.2.17"
lazy val nanohttpd = "org.nanohttpd" % "nanohttpd-websocket" % "2.3.1"
lazy val pegdown = "org.pegdown" % "pegdown" % "1.6.0"
lazy val scalatags = "com.lihaoyi" %% "scalatags" % "0.6.7"

lazy val main = (project in file("."))
  .settings(
    name := "SandScript",
    libraryDependencies ++= Seq(scalatest, nanohttpd, scalatags, pegdown, slf4j12, log4j),
  )
