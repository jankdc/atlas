lazy val root = (project in file(".")).
  settings(
    name := "atlas",
    version := "0.1.0",
    scalaVersion := "2.11.5"
  )

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3"
libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3"