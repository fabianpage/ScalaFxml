scalaVersion := "2.10.1"

scalacOptions ++= Seq(
    "-feature",
    "-deprecation"
)

parallelExecution := true

name := "ScalaFxml"

resolvers ++= Seq(
    "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
    "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
    "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
    "com.eed3si9n" %% "treehugger" % "0.2.3",
    "org.scalaz" %% "scalaz-core" % "7.0.0"
)

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "1.14" % "test",
    "org.scalatest" %% "scalatest" % "1.9.1" % "test",
    "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
    "junit" % "junit" % "4.11" % "test",
    "org.pegdown" % "pegdown" % "1.2.0" % "test"
)

testOptions in Test += Tests.Argument("junitxml", "html", "console", "markup")