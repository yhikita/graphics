name := """graphics"""

organization := "com.ruimo"

scalaVersion := "2.11.8"

resolvers += "ruimo.com" at "http://static.ruimo.com/release"

publishTo := Some(
  Resolver.file(
    "graphics",
    new File(Option(System.getenv("RELEASE_DIR")).getOrElse("/tmp"))
  )
)

// Change this to another test framework if you prefer
libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.8.2" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"

// Logging
libraryDependencies ++= Seq(
  "com.ruimo" %% "scoins" % "1.10",
  "ch.qos.logback" % "logback-core" % "1.1.3",
  "ch.qos.logback" % "logback-classic" % "1.1.3",
  "org.slf4j" % "slf4j-api" % "1.7.12"
)
