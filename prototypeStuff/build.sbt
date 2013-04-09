scalaVersion := "2.10.1"

fork := true

scalacOptions ++= Seq(
    "-feature",
    "-deprecation"
)

resolvers ++= Seq(
    "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
    "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
    "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
    "org.scalafx" %% "scalafx" % "1.0.0-M2",
    "com.eed3si9n" %% "treehugger" % "0.2.3",
    "org.scalaz" %% "scalaz-core" % "7.0.0-RC1"
)

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "1.14" % "test",
    "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
    "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"
)

unmanagedJars in Compile += Attributed.blank(file("/Library/Java/JavaVirtualMachines/jdk1.7.0_11.jdk/Contents/Home/jre/lib/jfxrt.jar"))