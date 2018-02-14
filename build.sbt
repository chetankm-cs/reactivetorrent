import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.chetankm",
      scalaVersion := "2.12.3",
      version := "0.1.0-SNAPSHOT"
    )),
    name := "reactivetorrent",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % "2.5.9",
      "com.typesafe.akka" %% "akka-testkit" % "2.5.9" % Test,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0",
      "commons-codec" % "commons-codec" % "1.11",
      "com.typesafe.play" %% "play-ahc-ws-standalone" % "1.1.3",
      scalaTest % Test
    )
  )
