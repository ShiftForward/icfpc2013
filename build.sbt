name := "icpfpc2013"

organization := "guru.meditation"

version := "0.1"

description := "ICFP Contest 2013"

startYear := Some(2013)

scalaVersion := "2.10.2"

scalacOptions := Seq("-deprecation", "-encoding", "utf8")

resolvers += "spray repo" at "http://repo.spray.io"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor"   % "2.2.0-RC1",
  "io.spray"           % "spray-client" % "1.2-M8",
  "io.spray"          %% "spray-json"   % "1.2.5")
