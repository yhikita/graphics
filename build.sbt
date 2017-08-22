name := """graphics"""

organization := "com.ruimo"

crossScalaVersions := List("2.11.8", "2.12.3") 

scalaVersion := "2.12.3"

resolvers += "ruimo.com" at "http://static.ruimo.com/release"

publishTo := Some(
  Resolver.file(
    "graphics",
    new File(Option(System.getenv("RELEASE_DIR")).getOrElse("/tmp"))
  )
)

scalacOptions in Test ++= Seq("-Yrangepos")

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"

// Logging
libraryDependencies ++= Seq(
  "com.ruimo" %% "scoins" % "1.12",
  "ch.qos.logback" % "logback-core" % "1.1.3",
  "ch.qos.logback" % "logback-classic" % "1.1.3",
  "org.slf4j" % "slf4j-api" % "1.7.12",
  "org.specs2" %% "specs2-core" % "3.9.4" % "test"
)
