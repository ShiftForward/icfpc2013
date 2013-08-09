name := "icpfpc2013"

organization := "guru.meditation"

version := "0.1"

description := "ICFP Contest 2013"

startYear := Some(2013)

scalaVersion := "2.10.2"

scalacOptions := Seq("-deprecation", "-encoding", "utf8")

libraryDependencies ++= Seq(
  "io.spray" %%  "spray-json" % "1.2.5")
