scalaVersion := "2.12.9"

val Http4sVersion = "0.20.9"
val CirceVersion = "0.11.1"
val LogbackVersion = "1.2.3"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.6.1",
  "org.typelevel" %% "cats-testkit" % "1.6.1",
  "org.typelevel" %% "cats-effect" % "1.3.1",
  "dev.zio" %% "zio" % "1.0.0-RC11-1",
  "dev.zio" %% "zio-interop-cats" % "2.0.0.0-RC2",
  "io.monix" %% "monix" % "3.0.0-RC3",
  "org.http4s" %% "http4s-blaze-server" % Http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % Http4sVersion,
  "org.http4s" %% "http4s-circe" % Http4sVersion,
  "org.http4s" %% "http4s-dsl" % Http4sVersion,
  "io.circe" %% "circe-generic" % CirceVersion,
  "ch.qos.logback" % "logback-classic" % LogbackVersion
)
scalacOptions += "-Ypartial-unification"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")
