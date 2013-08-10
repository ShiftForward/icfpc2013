name := "icpfpc2013"

organization := "guru.meditation"

version := "0.1"

description := "ICFP Contest 2013"

startYear := Some(2013)

scalaVersion := "2.10.2"

scalacOptions := Seq("-deprecation", "-encoding", "utf8")

resolvers ++= Seq(
  "spray repo" at "http://repo.spray.io",
  "spray nightlies repo" at "http://nightlies.spray.io")


libraryDependencies ++= Seq(
  "com.typesafe.akka"   %% "akka-actor"    % "2.2.0",
  "io.spray"             % "spray-client"  % "1.2-20130710",
  "org.apache.commons"   % "commons-math3" % "3.2",
  "io.spray"            %% "spray-json"    % "1.2.5")

initialCommands in console := """
      import icfpc2013.Client._;
      import spray.util._;
      import icfpc2013._;"""
