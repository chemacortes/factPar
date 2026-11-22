ThisBuild / scalaVersion := "3.5.2"
ThisBuild / semanticdbEnabled := true

libraryDependencies ++= Seq(
  "io.github.hughsimpson" %% "scalameter" % "0.22.1",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.3.0"
)
