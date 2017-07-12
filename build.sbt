scalaVersion := "2.11.8"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.6",
  "org.typelevel" %% "scodec-bits" % "1.0.0",
  "org.scalaz" %% "scalaz-concurrent" % "7.0.1",
  "org.scalaz.stream" %% "scalaz-stream" % "0.8.5",
  "org.scalaz" %% "scalaz-effect" % "7.2.8",
  "joda-time" % "joda-time" % "2.6",
  "org.joda" % "joda-convert" % "1.8",
  "com.google.guava" % "guava" % "17.0"
)