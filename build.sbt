lazy val root = (project in file("."))
  .settings(
    name := "training",
    scalaVersion := "2.13.1"
  )

  libraryDependencies += "org.specs2" %% "specs2-core" % "4.9.4"

  scalacOptions in Test ++= Seq("-Yrangepos")