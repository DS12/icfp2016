name := "ICFP"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalaj" %% "scalaj-http" % "2.3.0"

libraryDependencies += "org.typelevel" %% "cats" % "0.6.1"

// https://mvnrepository.com/artifact/com.typesafe.play/play-json_2.11
libraryDependencies += "com.typesafe.play" % "play-json_2.11" % "2.5.3"

// visualization libarary
libraryDependencies += "com.github.wookietreiber" %% "scala-chart" % "latest.integration"

scalacOptions += "-Ylog-classpath"