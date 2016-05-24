name := """graphics"""

organization := "com.ruimo"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.7"

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

