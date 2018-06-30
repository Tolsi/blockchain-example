name := "blockchain-example"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "com.typesafe.akka" %% "akka-actor" % "2.5.11",
  "com.typesafe.akka" % "akka-stream_2.12" % "2.5.11",
  "com.typesafe.akka" %% "akka-slf4j" % "2.5.11",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.11" % "test",
  "com.typesafe.akka" %% "akka-http" % "10.1.0",
  "com.typesafe.akka" %% "akka-http-testkit" % "10.1.0" % "test",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.0",
  "org.scorexfoundation" %% "scrypto" % "2.1.2")