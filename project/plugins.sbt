resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

resolvers += Classpaths.sbtPluginReleases

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.4.0")

addSbtPlugin("io.spray" % "sbt-revolver" % "0.6.2")

addSbtPlugin("me.lessis" % "sbt-growl-plugin" % "0.1.3")

