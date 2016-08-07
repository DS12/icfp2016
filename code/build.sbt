name := "ICFP"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalaj" %% "scalaj-http" % "2.3.0"

libraryDependencies += "org.typelevel" %% "cats" % "0.6.1"

// https://mvnrepository.com/artifact/com.typesafe.play/play-json_2.11
libraryDependencies += "com.typesafe.play" % "play-json_2.11" % "2.5.3"

// visualization libarary
libraryDependencies += "com.github.wookietreiber" %% "scala-chart" % "latest.integration"

// for Rational
libraryDependencies += "org.spire-math" %% "spire" % "0.11.0"

// for test
val scalaTestVersion = "3.0.0-M15"
libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion