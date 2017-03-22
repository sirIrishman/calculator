lazy val root = (project in file("."))
  .settings(
    name         := "calculator",
    organization := "siririshman",
    scalaVersion := "2.12.1",
    version      := "0.2.0"
  )
  resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
  libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  logBuffered in Test := false
