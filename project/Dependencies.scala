import sbt._

object Dependencies {

  val resolutionRepos = Seq(
    "Sonatype OSS Releases"   at "http://oss.sonatype.org/content/repositories/releases/",
    "Typesafe"                at "http://repo.typesafe.com/typesafe/releases/",
    "Artima Maven Repository" at "http://repo.artima.com/releases"
  )

  object V {
    // Scala
    val json4s               = "3.2.11"
    val pureconfig           = "0.8.0"
    // Scala (test only)
    val scalactic            = "3.0.5"
  }

  val Libraries = Seq(
    // Scala
    "org.scalactic"         %% "scalactic"    % V.scalactic,
    "com.github.pureconfig" %% "pureconfig"   % V.pureconfig,

    // Scala (test only)
    "org.scalatest"         %% "scalatest"    % V.scalactic   % "test"
  )
}
