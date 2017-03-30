name := "pdf4s"
organization := "org.matthicks"
version := "1.0.0-SNAPSHOT"
scalaVersion := "2.12.1"
crossScalaVersions := List("2.12.1", "2.11.8")
sbtVersion := "0.13.13"
fork := true

libraryDependencies += "com.itextpdf" % "itextpdf" % "5.5.11"
