organization := "com.optrak"

name := "json-shapeless-21"

version := "0.7.0-SNAPSHOT"

resolvers ++= Seq(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "sonatype.releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  "SS" at "http://oss.sonatype.org/content/repositories/snapshots/",
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases")

scalaVersion := "2.11.4"

libraryDependencies ++= {
  Seq(
    "org.json4s" %% "json4s-native" % "3.2.11",
    "com.chuusai" %% "shapeless" % "2.1.0-SNAPSHOT" changing()
  )
}

parallelExecution in Test := true

exportJars := true

scalacOptions ++= Seq("-feature", "-deprecation")
