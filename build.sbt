import com.github.retronym.SbtOneJar._

oneJarSettings

lazy val root = (project in file(".")).
  settings(
    name := "atlas",
    scalaVersion := "2.11.5"
  )

