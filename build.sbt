val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "eltonsst",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    resourceDirectory in Compile := baseDirectory.value / "resources"
  )
