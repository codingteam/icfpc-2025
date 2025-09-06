scalaVersion := "3.7.2"
libraryDependencies ++= Seq(
    "com.softwaremill.sttp.client4" %% "core" % "4.0.10",
    "com.softwaremill.sttp.client4" %% "upickle" % "4.0.10",
    "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

scalacOptions ++= Seq("-explain")
