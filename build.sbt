import com.github.retronym.SbtOneJar._

oneJarSettings

lazy val root = (project in file(".")).
  settings(
    name := "atlas",
    version := "0.1.0",
    scalaVersion := "2.11.5"
  )

