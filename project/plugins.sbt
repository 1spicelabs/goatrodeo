resolvers += Resolver.bintrayIvyRepo("rallyhealth", "sbt-plugins")
addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.6.0")
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.10.4")
addSbtPlugin("com.rallyhealth.sbt" % "sbt-git-versioning" % "1.6.0")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.6")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.14.2")
