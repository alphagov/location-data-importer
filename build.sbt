name := "location-data-importer"

version := "0.1"

scalaVersion := "2.10.0"

parallelExecution := false

libraryDependencies ++= Seq(
    "org.geotools" % "gt-main" % "2.7.0.1",
    "org.geotools" % "gt-epsg-hsql" % "2.7.0.1",
    "org.geotools" % "gt-xml" % "2.7.0.1",
    "com.github.scopt" %% "scopt" % "3.2.0",
    "com.github.scala-incubator.io" % "scala-io-core_2.10.2" % "0.4.2",
    "ch.qos.logback" % "logback-classic" % "1.0.3",
    "joda-time" % "joda-time" % "2.3",
    "org.joda" % "joda-convert" % "1.2",
    "org.mongodb" %% "casbah" % "2.5.0",
    "com.novus" % "salat-core_2.10" % "1.9.2",
    "org.specs2" %% "specs2" % "2.3.3" % "test"
    )

resolvers ++= Seq(
    "maven" at "http://download.java.net/maven/2",
    "geotools" at "http://download.osgeo.org/webdav/geotools",
    "sonatype-public" at "https://oss.sonatype.org/content/groups/public",
    "GDS maven repo snapshots" at "http://alphagov.github.com/maven/snapshots")



