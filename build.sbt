name := "icpfpc2013"

organization := "guru.meditation"

version := "0.1"

description := "ICFP Contest 2013"

startYear := Some(2013)

scalaVersion := "2.10.2"

scalacOptions := Seq("-deprecation", "-encoding", "utf8", "-optimize")

resolvers ++= Seq(
  "spray repo" at "http://repo.spray.io",
  "spray nightlies repo" at "http://nightlies.spray.io")

libraryDependencies ++= Seq(
  "com.typesafe.akka"   %% "akka-actor"      % "2.2.0",
  "io.spray"             % "spray-client"    % "1.2-20130710",
  "org.apache.commons"   % "commons-math3"   % "3.2",
  "io.spray"            %% "spray-json"      % "1.2.5",
  "com.typesafe.slick"  %% "slick"           % "1.0.1",
  "org.slf4j"            % "slf4j-nop"       % "1.6.4",
  "com.twitter"         %% "chill"           % "0.3.1",
  "com.twitter"         %% "chill-bijection" % "0.3.1",
  "commons-io"           % "commons-io"      % "2.4")

initialCommands in console := """
      import icfpc2013.Client._;
      import spray.util._;
      import icfpc2013._;"""

fork in run := true

javaOptions in run ++= Seq("-Xmx4g", "-Xms4g", "-Xmn400M")
