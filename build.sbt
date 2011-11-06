
name := "logstyletest"

version := "1.0.0.0"

scalaVersion := "2.9.1"

mainClass in oneJar :=  Some("logstyletest")

libraryDependencies ++= Seq(
   "log4j" % "log4j" % "1.2.16"
)

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)