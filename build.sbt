lazy val root = project.in(file(".")).settings(
  name := "pdf4s",
  organization := "org.matthicks.pdf4s",
  version := "1.0.0",
  scalaVersion := "2.11.8",
  sbtVersion := "0.13.11",
  libraryDependencies += "com.itextpdf" % "itextpdf" % "5.5.9"
)

