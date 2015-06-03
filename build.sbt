import com.github.retronym.SbtOneJar._

oneJarSettings

lazy val root = (project in file(".")).
  settings(
    name := "atlas",
    scalaVersion := "2.11.5"
  )

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
