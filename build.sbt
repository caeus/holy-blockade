import Dependencies._


organization := "gdx.travel"
scalaVersion := "2.11.11"
version := "0.1.0-SNAPSHOT"

name := "Hello"


resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"))

libraryDependencies ++= Seq(scalaTest % Test,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  "com.chuusai" %% "shapeless" % "2.3.2"
)

