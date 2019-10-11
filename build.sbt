name := "functional-data-structures"

version := "0.1"

scalaVersion := "2.13.1"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)