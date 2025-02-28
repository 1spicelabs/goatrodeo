import java.nio.file.{FileAlreadyExistsException, Files, Paths}
import scala.sys.process._

val projectName = "goatrodeo"
val projectVersion = "0.6.2-SNAPSHOT"
val scala3Version = "3.6.3"

fork := true

ThisBuild / scalacOptions ++=
  Seq(
    "-deprecation",
    "-unchecked",
    "-Wunused:imports",
    "-feature",
    "-release",
    "21"
  )


resolvers += "OW2" at "https://repository.ow2.org/nexus/content/repositories/public/"
Test / logBuffered := false

lazy val root = project
  .in(file("."))
  .settings(
    name := projectName,
    scalaVersion := scala3Version,
    semanticdbEnabled := true, // enable SemanticDB,
    semanticdbVersion := scalafixSemanticdb.revision,
    libraryDependencies += "org.scala-lang" %% "toolkit" % "0.4.0",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.3.0",
    libraryDependencies += "org.apache.bcel" % "bcel" % "6.10.0",
    libraryDependencies += "com.github.scopt" %% "scopt" % "4.1.0",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "commons-io" % "commons-io" % "2.16.1",
    libraryDependencies += "io.bullet" %% "borer-core" % "1.14.1",
    libraryDependencies += "io.bullet" %% "borer-derivation" % "1.14.1",
    libraryDependencies += "com.palantir.isofilereader" % "isofilereader" % "0.6.1",

    // json4s
    libraryDependencies += "org.json4s" %% "json4s-native" % "4.0.7",
    libraryDependencies += "com.github.luben" % "zstd-jni" % "1.5.6-4",
    libraryDependencies += "org.apache.commons" % "commons-compress" % "1.27.1",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.5.15",
    libraryDependencies +=
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
    libraryDependencies += "org.apache.tika" % "tika-core" % "3.0.0",
    libraryDependencies +=  "com.github.package-url" % "packageurl-java" % "1.5.0",
    libraryDependencies += "org.tukaani" % "xz" % "1.10",
    assembly / mainClass := Some("goatrodeo.Howdy"),
    compileOrder := CompileOrder.JavaThenScala,
    scalacOptions += "-no-indent"
  )

ThisBuild / assemblyJarName := "goatrodeo.jar"

ThisBuild / assemblyMergeStrategy := {
  case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
  case _                                   => MergeStrategy.last
}

Test / testOptions += Tests.Setup(() => {
  val log = (streams.value: @sbtUnchecked).log
  log.info("Downloading and caching test data…")
  try {
    Files.createDirectories(Paths.get("test_data/download/iso_tests"))
  } catch {
    case fE: FileAlreadyExistsException =>
      log.info("\t! iso_tests directory already exists.")
    case e: Throwable =>
      val err = s"Exception setting up iso_tests directory: ${e.getMessage}"
      log.error(err)
      throw new MessageOnlyException(err)
  }

  try {
    Files.createDirectories(Paths.get("test_data/download/gem_tests"))
  } catch {
    case fE: FileAlreadyExistsException =>
    // log.info("\t! gem_tests directory already exists.")
    case e: Throwable =>
      val err = s"Exception setting up gem_tests directory: ${e.getMessage}"
      log.error(err)
      throw new MessageOnlyException(err)
  }

  try {

    Files.createDirectories(Paths.get("test_data/download/apk_tests"))
  } catch {
    case fE: FileAlreadyExistsException =>
    // log.info("\t! apk_tests directory already exists.")
    case e: Throwable =>
      val err = s"Exception setting up apk_tests directory: ${e.getMessage}"
      log.error(err)
      throw new MessageOnlyException(err)
  }

  try {

    Files.createDirectories(Paths.get("test_data/download/deb_tests"))
  } catch {
    case fE: FileAlreadyExistsException =>
      log.info("\t! deb_tests directory already exists.")
    case e: Throwable =>
      val err = s"Exception setting up deb_tests directory: ${e.getMessage}"
      log.error(err)
      throw new MessageOnlyException(err)
  }

  try {
    {
      val f = new File("./test_data/download/iso_tests/iso_of_archives.iso")
      if (!f.exists()) {
        log.info("\t* Fetching test ISOs…")
        url(
          "https://public-test-data.spice-labs.dev/iso_of_archives.iso"
        ) #> f ! log
      }
    }
    {
      val f = file("./test_data/download/iso_tests/simple.iso")
      if (!f.exists()) {
        url("https://public-test-data.spice-labs.dev/simple.iso") #> f ! log
      }
    }
    {
      val f = file("./test_data/download/gem_tests/java-properties-0.3.0.gem")
      if (!f.exists()) {
        log.info("\t * Fetching test Gems…")
        url(
          "https://public-test-data.spice-labs.dev/java-properties-0.3.0.gem"
        ) #> f ! log
      }
    }
    {
      val f = file("./test_data/download/sample-tomcat-6.war")
      if (!f.exists()) {
        log.info("\t * Fetching test WARs…")
        url(
          "https://public-test-data.spice-labs.dev/sample-tomcat-6.war"
        ) #> f ! log
      }
    }
    {
      val f = file("./test_data/download/EnterpriseHelloWorld.ear")
      if (!f.exists()) {
        log.info("\t * Fetching test EARs…")
        url(
          "https://public-test-data.spice-labs.dev/EnterpriseHelloWorld.ear"
        ) #> f ! log
      }
    }
    {
      val f = file("./test_data/download/apk_tests/bitbar-sample-app.apk")
      if (!f.exists()) {
        log.info("\t * Fetching test Android APKs…")
        url(
          "https://public-test-data.spice-labs.dev/bitbar-sample-app.apk"
        ) #> f ! log
      }
    }
    {
      val f = file("./test_data/download/deb_tests/hello_2.10-3_arm64.deb")
      if (!f.exists()) {
        log.info("\t * Fetching test Debian Packages…")
        url(
          "https://public-test-data.spice-labs.dev/hello_2.10-3_arm64.deb"
        ) #> f ! log
      }
    }
  } catch {
    case e: Throwable =>
      val err = s"Exception fetching test files: ${e.getMessage}"
      log.error(err)
      println(err)
      throw new MessageOnlyException(err)
  }
  log.info("Test data caching complete.")
})

// Verify that git LFS is installed and files are correct before running tests
Test / testOptions += Tests.Setup(() => {
  val log = (streams.value: @sbtUnchecked).log
  log.info("Testing for git LFS…")
  if ("git lfs status".! == 0) {
    log.info("git lfs found, proceeding…")
  } else {
    val err =
      "git lfs not found. Please review the README.md for setup instructions!"
    log.error(err)
    throw new MessageOnlyException(err)
  }
  {
    log.info("Running a `git lfs pull`…")
    if ("git lfs pull".! == 0) {
      log.info("git lfs files should all be synced now.")
    } else {
      val err = "`git lfs pull` failed!"
      log.error(err)
      throw new MessageOnlyException(err)
    }
  } 
})

enablePlugins(JavaAppPackaging)
enablePlugins(GitVersioningPlugin)
enablePlugins(DockerPlugin)

Universal / mappings += file("data/grim.json") -> "data/grim.json"
Docker / packageName := projectName
Docker / maintainer := "ext-engineering@spicelabs.io"

dockerBaseImage := "eclipse-temurin:21-jre-ubi9-minimal"
dockerLabels := Map.empty
dockerExposedPorts := Seq.empty
