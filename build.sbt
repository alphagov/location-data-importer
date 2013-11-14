name := "location-data-importer"

version := "0.1"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
    "com.github.scopt" %% "scopt" % "3.2.0",
    "com.github.scala-incubator.io" % "scala-io-core_2.10.2" % "0.4.2",
    "ch.qos.logback" % "logback-classic" % "1.0.3",
    "joda-time" % "joda-time" % "2.3",
    "org.joda" % "joda-convert" % "1.2",
    "org.specs2" %% "specs2" % "2.3.3" % "test"
    )

resolvers ++= Seq(
    "sonatype-public" at "https://oss.sonatype.org/content/groups/public",
    "GDS maven repo snapshots" at "http://alphagov.github.com/maven/snapshots")



