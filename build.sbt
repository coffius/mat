lazy val root = (project in file(".")).
  settings(
	organization := "io.koff",
    name := "mat",
    version := "0.0.1",
    scalaVersion := "2.11.7"
  )

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)