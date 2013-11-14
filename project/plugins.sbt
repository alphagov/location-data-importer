resolvers ++= Seq(
    "sbt-idea-repo" at "http://mpeltonen.github.com/maven/",
     "Typesafe releases" at "http://repo.typesafe.com/typesafe/releases",
        "GDS maven repo snapshots" at "http://alphagov.github.com/maven/snapshots",
        "GDS maven repo releases" at "http://alphagov.github.com/maven/releases",
        "Java.net Maven2 Repository" at "http://download.java.net/maven/2/",
        "repo.novus snaps" at "http://repo.novus.com/snapshots/",
        "repo.codahale" at "http://repo.codahale.com",
        "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
        "repo scalatools releases" at "https://oss.sonatype.org/content/groups/scala-tools/",
        "sbt-assembly-resolver-0" at "http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"
)

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.5.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.2.1")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.9.1")


