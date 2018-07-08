import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "money-machine",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "org.apache.poi" % "poi-ooxml" % "3.9",
      "com.lightbend.akka" %% "akka-stream-alpakka-elasticsearch" % "0.20"
    )
  )
