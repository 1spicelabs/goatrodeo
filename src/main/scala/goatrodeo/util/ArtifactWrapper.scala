package goatrodeo.util

import com.typesafe.scalalogging.Logger
import org.apache.tika.config.TikaConfig
import org.apache.tika.metadata.Metadata
import org.apache.tika.metadata.TikaCoreProperties

import java.io.BufferedInputStream
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.InputStream
import java.nio.file.Files
import java.nio.file.Path
import java.util.UUID

/** In OmniBOR, everything is seen as a byte stream.
  *
  * `ArtifactWrapper` provides a wrapper around artifacts... byte streams.
  *
  * `ArtifactWrapper` is somewhat modeled on the JVM `java.io.File` class so
  * that it can represent both files on a filesystem with directories and
  * virtual files contained in a Zip or TAR or other container file.
  *
  * Those byte streams may be in-memory or they may be on disk.
  */
sealed trait ArtifactWrapper {

  /** Convert the Artifact to a stream of bytes. Note that this is mostly used
    * for Hashing which is a block operation. No need for any buffering because
    * all the operations will be pulling large blocks of bytes.
    *
    * @return
    */
  def asStream(): InputStream

  /** The name of the Artifact. This corresponds to the name of a `File` on disk
    *
    * @return
    */
  def path(): String

  /** The number of bytes in the artifact.
    *
    * @return
    */
  def size(): Long

  private lazy val _mimeType: String =
    ArtifactWrapper.mimeTypeFor(this.asStream(), this.path())

  def mimeType: String = _mimeType

  lazy val uuid: String = UUID.randomUUID().toString()

  /** Get the name for the file irregarless of the path
    */
  lazy val filenameWithNoPath: String = (new File(path())).getName()

  protected def fixPath(p: String): String = ArtifactWrapper.fixPath(p)
}

object ArtifactWrapper {
  private val logger = Logger(getClass())
  // max in memory size 32MB
  val maxInMemorySize: Long = 32L * 1024 * 1024;
  private val tika = new TikaConfig()

  /** Given an input stream and a filename, get the mime type
    *
    * @param data
    *   -- the input stream
    * @param fileName
    *   -- the name of the file
    */
  def mimeTypeFor(rawData: InputStream, fileName: String): String = {
    try {
      val data = new BufferedInputStream(rawData)

      val metadata = new Metadata()
      metadata.set(TikaCoreProperties.RESOURCE_NAME_KEY, fileName)
      val detected = tika.getDetector.detect(data, metadata)

      detected.toString()
    } catch {
      case e: Exception =>
        logger.error(
          f"Tika failed, ${e.getMessage()}. Returning application/octet-stream",
          e
        )
        "application/octet-stream"
    }
  }

  protected def fixPath(p: String): String = {
    if (p.startsWith("./")) fixPath(p.substring(2))
    else if (p.startsWith("/")) fixPath(p.substring(1))
    else if (p.startsWith("../")) fixPath(p.substring(3))
    else p
  }

  /** Give some data, create a new wrapper for it
    *
    * @param nominalPath
    *   the path
    * @param size
    *   the size of the data
    * @param data
    *   the InputStream of the data
    * @param tempPath
    *   the temporary directory to put a file in
    *
    * @return
    *   the built item
    */
  def newWrapper(
      nominalPath: String,
      size: Long,
      data: InputStream,
      tempPath: Path
  ): ArtifactWrapper = {
    val name = fixPath(nominalPath)
    if (size <= maxInMemorySize) {
      val bos = ByteArrayOutputStream()
      Helpers.copy(data, bos)
      val bytes = bos.toByteArray()
      if (size != bytes.length) {
        throw Exception(
          f"Failed to create wrapper for ${name} expecting ${size} bytes, but got ${bytes.length}"
        )
      }
      ByteWrapper(bytes, name)
    } else {
      val tempFile = Files.createTempFile(tempPath, "goats", ".temp").toFile()
      val fos = FileOutputStream(tempFile)
      Helpers.copy(data, fos)
      fos.flush()
      fos.close()
      FileWrapper(tempFile, name)
    }
  }
}

final case class FileWrapper(f: File, thePath: String) extends ArtifactWrapper {
  if (!f.exists()) {
    throw Exception(
      f"Tried to create file wrapper for ${f.getCanonicalPath()} that does not exist"
    )
  }
  override def asStream(): InputStream = new BufferedInputStream(
    FileInputStream(f)
  )

  override def path(): String = fixPath(thePath)

  override def size(): Long = f.length()

}

object FileWrapper {
  def fromName(name: String): FileWrapper = {
    FileWrapper(new File(name), name)
  }
}

final case class ByteWrapper(bytes: Array[Byte], fileName: String)
    extends ArtifactWrapper {

  override def asStream(): BufferedInputStream = new BufferedInputStream(
    ByteArrayInputStream(bytes)
  )

  override def path(): String = fileName

  override def size(): Long = bytes.length
}
