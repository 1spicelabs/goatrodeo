import com.typesafe.scalalogging.Logger
import org.apache.tika.config.TikaConfig
import org.apache.tika.io.TikaInputStream
import org.apache.tika.metadata.Metadata
import org.apache.tika.metadata.TikaCoreProperties
import org.apache.tika.mime.MediaType
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import io.spicelabs.goatrodeo.util.filetypes.*
import org.apache.tika.parser.ParseContext
import org.apache.tika.sax.BodyContentHandler
import org.scalatest.TryValues._

import io.spicelabs.goatrodeo.util.filetypes

import java.io.{File, FileInputStream}

class FiletypeDetectionAndMetadataSuite extends AnyFlatSpec with Matchers {
  import MIMETypeMappings._

  val tika = new TikaConfig()
  val logger = Logger("FiletypeDetectionAndMetadataSuite")


  def detectFiletype(f: File): MediaType = {
    val metadata = new Metadata()
    metadata.set(TikaCoreProperties.RESOURCE_NAME_KEY, f.toString)
    val detected = tika.getDetector.detect(TikaInputStream.get(f), metadata)
    println(s"Detected filetype for ${f.toString} media type: $detected Main Type: ${detected.getType} Subtype: ${detected.getSubtype}")
    detected
  }
  // File formats that are packaged in zip (jar, war, ear, apk, etc.)
  "A plain old .zip file" must "be detected" in {
    val zip1 = new File("test_data/HP1973-Source.zip")
    detectFiletype(zip1) mustBe MIME_ZIP
  }

  it must "extract the ZIP Metadata (if any)" in {

  }

  "A .jar file" must "be detected as such" in {
    val jar1 = new File("test_data/hidden1.jar")
    detectFiletype(jar1) mustBe MIME_JAR

    val jar2 = new File("test_data/hidden2.jar")
    detectFiletype(jar2) mustBe MIME_JAR

    val jar3 = new File("test_data/log4j-core-2.22.1.jar")
    detectFiletype(jar3) mustBe MIME_JAR
  }

  it must "extract the Jar metadata" in {
  }

  "A .war file" must "be detected as such" in {
    val war1 = new File("test_data/sample-tomcat-6.war")
    detectFiletype(war1) mustBe MIME_WAR
  }

  it must "extract the WAR Metadata" in {

  }

  "An .ear file" must "be detected as such" in {
    val ear1 = new File("test_data/EnterpriseHelloWorld.ear")
    detectFiletype(ear1) mustBe MIME_EAR
  }

  it must "extract the EAR Metadata" in {

  }


  "An .iso file" must "be detected as such" in {
    val iso1 = new File("test_data/iso_tests/iso_of_archives.iso")
    val iso2 = new File("test_data/iso_tests/simple.iso")
    detectFiletype(iso1) mustBe MIME_ISO
    detectFiletype(iso2) mustBe MIME_ISO
  }

  it must "extract the ISO Metadata" in {

  }

  // OS Packages
  "A .deb file" must "be detected as such" in {
    val deb1 = new File("test_data/tk8.6_8.6.14-1build1_amd64.deb")
    detectFiletype(deb1) mustBe MIME_DEB
  }

  it must "extract the DEB Metadata" in {
    // this is still rough / experimental we probably want to use the AutoDetect parser stuff at some point…
    val f = new File("test_data/tk8.6_8.6.14-1build1_amd64.deb")
    val meta = MIMETypeMappings.resolveMetadata(f)
    logger.info(s"Got metadata: $meta")
    meta.success.value mustBe Map("deb" -> "debian", "foo" -> "bar")
  }

  "An .rpm file" must "be detected as such" in {
    val rpm1 = new File("test_data/tk-8.6.8-1.el8.x86_64.rpm")
    detectFiletype(rpm1) mustBe MIME_RPM
  }

  it must "extract the RPM Metadata" in {

  }

  // Tar based formats
  "A .gem file" must "be detected as such" in {
    val gem1 = new File("test_data/gem_tests/java-properties-0.3.0.gem")
    detectFiletype(gem1) mustBe MIME_GEM
  }

  it must "extract the GEM Metadata" in {

  }

  "An android .apk archive" must "be detected as such" in {
    val apk1 = new File("./test_data/apk_tests/bitbar-sample-app.apk")
    val apk2 = new File("./test_data/tk-8.6.13-r2.apk") // this one just detects as a gzip, it looks like it might be an ALPINE apk…
    detectFiletype(apk1) mustBe MIME_APK
    //detectFiletype(apk2).toString mustBe MIME_APK
  }

  it must "extract the APK Metadata" in {

  }

  "A regular old .tar file" must "be detected as such" in {
    val tar1 = new File("./test_data/nested.tar")
    val tar2 = new File("./test_data/hidden.tar")
    val tar3 = new File("./test_data/ics_test.tar")
    detectFiletype(tar1) mustBe MIME_TAR
    detectFiletype(tar2) mustBe MIME_TAR
    detectFiletype(tar3) mustBe MIME_TAR
  }

  it must "extract the TAR Metadata (if any)" in {

  }

  "A tarball (.tar.gz / .tgz) " must "be detected as such" in {
    val gz1 = new File("test_data/empty.tgz")
    detectFiletype(gz1) mustBe MIME_GZIP
  }

  it must "extract the Tarball Metadata (if any)" in {

  }

  // todo - define "appropriately"
  "An unknown file type" must "be handled appropriately" in {


  }
}
