package goatrodeo.util

import com.palantir.isofilereader.isofilereader.IsoFileReader
import com.typesafe.scalalogging.Logger
import org.apache.commons.compress.archivers.ArchiveEntry
import org.apache.commons.compress.archivers.ArchiveInputStream
import org.apache.commons.compress.archivers.ArchiveStreamFactory
import org.apache.commons.compress.compressors.CompressorInputStream
import org.apache.commons.compress.compressors.CompressorStreamFactory

import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.InputStream
import java.nio.file.Files
import java.nio.file.Path
import java.util.zip.ZipFile
import scala.util.Try

enum FileAction {
  case SkipDive
  case End
}
object FileWalker {
  val logger: Logger = Logger(getClass())

  private lazy val zipMimeTypes: Set[String] =
    Set(
      "application/zip",
      "application/java-archive",
      "application/vnd.android.package-archive"
    )

  private lazy val isoMimeTypes: Set[String] = Set(
    "application/x-iso9660-image"
  )

  /** Try to construct an `OptionalArchiveStream` from a Zip/WAR/etc. file
    *
    * @param in
    *   the file to try to construct the stream from
    * @return
    */
  private def asZipContainer(
      in: ArtifactWrapper,
      tempDir: Path
  ): OptionalArchiveStream = {
    if (zipMimeTypes.contains(in.mimeType)) {
      import scala.jdk.CollectionConverters.IteratorHasAsScala

      val theFile = in match {
        case FileWrapper(f, _) => f
        case _ => Helpers.tempFileFromStream(in.asStream(), true, tempDir)
      }

      try {

        val zipFile = ZipFile(theFile)
        val it: Vector[ArtifactWrapper] = zipFile
          .stream()
          .iterator()
          .asScala
          .filter(v => { !v.isDirectory() })
          .map(v => {
            val name = v.getName()
            val size = v.getSize()
            ArtifactWrapper
              .newWrapper(name, size, zipFile.getInputStream(v), tempDir)
          })
          .toVector
        zipFile.close()
        Some(
          it,
          "Zip Container"
        )

      } catch {
        case e: Exception =>
          // if creating the artfiact wrappers from the thing that might be a zip file fail, it
          // just means it's not a valid zip. This is okay.
          logger.trace(
            f"Failed for ${in.path()} mime type ${in.mimeType} error ${e.getMessage()}",
            e
          )
          None
      }
    } else {
      None
    }
  }

  /** Try to open the thing as an ISO file wrapper
    *
    * @param in
    * @return
    */
  private def asISOWrapper(
      in: ArtifactWrapper,
      tempPath: Path
  ): OptionalArchiveStream = {
    if (isoMimeTypes.contains(in.mimeType)) {
      try {
        val theFile = in match {
          case FileWrapper(f, _) => f
          case _ => Helpers.tempFileFromStream(in.asStream(), true, tempPath)
        }

        val isoFileReader: IsoFileReader = new IsoFileReader(theFile)

        try {

          val files = isoFileReader.getAllFiles()

          import scala.jdk.CollectionConverters.ListHasAsScala
          val flatList =
            isoFileReader.convertTreeFilesToFlatList(files).asScala.toVector

          val wrappers =
            for (cycleFile <- flatList)
              yield {

                val nameWithThing = cycleFile.getFullFileName('/')
                val name =
                  (if (nameWithThing.endsWith(";1")) {
                     nameWithThing.substring(0, nameWithThing.length() - 2)

                   } else nameWithThing).toLowerCase()

                val size = cycleFile.getSize()

                ArtifactWrapper.newWrapper(
                  name,
                  size,
                  isoFileReader.getFileStream(cycleFile),
                  tempPath
                )
              }

          isoFileReader.close()

          Some(
            wrappers,
            "ISO Reader"
          )

        } catch {
          case e: Exception =>
            // if creating the artfiact wrappers from the thing that might be an iso file fail, it
            // just means it's not a valid iso. This is okay.
            logger.trace(s"Try getAllFiles failed: ${e.getMessage}", e)
            isoFileReader.close()
            None
        }
      } catch {
        case e: Exception =>
          // if creating the artfiact wrappers from the thing that might be a zip file fail, it
          // just means it's not a valid zip. This is okay.
          logger.trace(s"Whole ISO Fetch process failed… ${e.getMessage}", e)
          None
      }
    } else None
  }

  /** Try to construct an `OptionalArchiveStream` using the Apache Commons "we
    * read most archive files" format
    *
    * Note this only reads *archives* – that is to say, something like a
    * `.tar.gz` with many files It will *not* work on simple single compressed
    * files in a gzip wrapper, which needs the `CompressorStreamFactory` rather
    * than the `ArchiveStreamFactory`
    *
    * @param in
    *   the file to try to construct the stream from
    * @return
    */
  private def asApacheCommonsArchiveWrapper(
      in: () => InputStream,
      name: String,
      tempDir: Path
  ): OptionalArchiveStream = {
    val factory = (new ArchiveStreamFactory())

    val fis = in()

    try {
      val input: ArchiveInputStream[ArchiveEntry] = factory
        .createArchiveInputStream(
          fis
        )
      val theIterator = Helpers
        .iteratorFor(input)
        .filter(!_.isDirectory())
        .map(ae => {
          val artifactName = ae.getName()

          val size = ae.getSize()

          ArtifactWrapper.newWrapper(artifactName, size, input, tempDir)
        })
        .toVector
      input.close()

      Some(theIterator -> "Apache Common Wrapper")
    } catch {
      case e: Exception =>
        fis.close(); None
    }
  }

  val definitelyNotArchive: Set[String] = Set(
    "application/java-vm",
    "text/plain",
    "multipart/appledouble",
    "application/xml-dtd",
    "application/xml",
    "text/x-java-properties",
    "text/x-scala",
    "text/x-java-source",
    "application/x-sh",
    "application/x-sharedlib",
    "image/png",
    "application/vnd.ms-fontobject",
    "application/x-font-ttf",
    "image/vnd.microsoft.icon",
    "image/gif",
    "application/x-sqlite3",
    "application/x-msdownload; format=pe64",
    "application/x-msdownload",
    "application/x-msdownload; format=pe32",
    "image/jpeg",
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    "audio/mpeg",
    "application/rdf+xml",
    "application/xhtml+xml",
    "image/svg+xml",
    "application/x-bat",
    "application/wsdl+xml",
    "application/x-mspublisher",
    "application/octet-stream",
    "application/x-dtbresource+xml",
    "application/vnd.iccprofile",
    "multipart/appledouble"
  )

  val notZip: Set[String] = Set(
    "application/x-rpm",
    "application/x-archive",
    "application/x-iso9660-image",
    "application/x-gtar",
    "application/x-debian-package",
    "application/gzip",
    "application/zstd"
  )

  val notCompressedSet: Set[String] = Set[String](
    "application/vnd.android.package-archive"
    // "application/x-debian-package",
    // "application/gzip",
    // "application/x-gtar"
  )

  def notArchive(in: ArtifactWrapper): Boolean =
    notArchive(in.path(), in.mimeType)

  def notArchive(path: String, mimeType: String): Boolean = {
    mimeType.startsWith("text/") ||
    mimeType.startsWith("image/") ||
    definitelyNotArchive.contains(mimeType) ||
    (mimeType == "application/zip" && path.endsWith(".xpi"))
  }

  def notCompressed(path: String, mimeType: String): Boolean = {
    mimeType.startsWith("text/") ||
    mimeType.startsWith("image/") ||
    notCompressedSet.contains(mimeType)
  }

  /** Try a series of strategies (except for uncompressing a file) for creating
    * an archive stream
    *
    * @param in
    *   the artifact wrapper to attempt to create a stream for
    * @return
    *   if the stream is created, an iterator of fuctions to access each stream
    *   piece and a closing/cleanup function which *must* be called
    */
  private def tryToConstructArchiveStream(
      in: ArtifactWrapper,
      tempDir: Path
  ): OptionalArchiveStream = {
    val mimeType = in.mimeType
    if (notArchive(in)) None
    else {
      {
        val ret = asZipContainer(in, tempDir)

        ret
      }.orElse(asISOWrapper(in, tempDir).orElse {
        // the inputstream to the apache stuff is either a
        // decompressed file or the input stream from the artifact
        val inputStreamBuilder: () => InputStream =
          (if (notCompressed(in.path(), in.mimeType)) None
           else {
             Try {

               try {
                 val factory = new CompressorStreamFactory()
                 val fis = new BufferedInputStream(in.asStream())

                 val input: CompressorInputStream =
                   factory.createCompressorInputStream(fis)

                 val theFile =
                   Files
                     .createTempFile(tempDir, "goats", "uncompressed")
                     .toFile()

                 val fos = new FileOutputStream(theFile)
                 Helpers.copy(input, fos)
                 fos.close()

                 () => new BufferedInputStream(new FileInputStream(theFile))
               } catch {
                 case e: Exception =>
                   if (
                     /*true ||*/ (in.mimeType != "application/vnd.android.package-archive" && (
                       in.path().endsWith(".tgz") ||
                         in.path().endsWith(".apk") ||
                         in
                           .path()
                           .endsWith(".xz")
                     ))
                   ) {
                     logger.error(
                       f"Expected to open apk/tgz/xz ${in
                           .path()} -- ${in.mimeType} but got ${e.getMessage()}",
                       e
                     )
                   }
                   throw e
               }
             }.toOption
           })
            .getOrElse(() => new BufferedInputStream(in.asStream()))

        asApacheCommonsArchiveWrapper(
          inputStreamBuilder,
          in.path(),
          tempDir
        )
      })
    }
  }

  /** If the Artifact is a "container" (e.g., tar file, zip file, etc.) then
    * expand the contained artifacts and traverse into the container with a
    * function.
    *
    * @param artifact
    *   -- the artifact to potentially traverse into
    * @param function
    *   -- the function that does "a thing" with the expanded artifacts
    *
    * @return
    *   if the thing is a container, then `Some(T)` where T is the value
    *   returned from `function`
    */
  def withinArchiveStream[T](artifact: ArtifactWrapper)(
      function: Vector[ArtifactWrapper] => T
  ): Option[T] = {
    if (notArchive(artifact)) None
    else {
      // create temporary directory
      val tempDir: Path = Files.createTempDirectory("goatrodeo_temp_dir")

      try {
        tryToConstructArchiveStream(artifact, tempDir) match {
          case None => None
          case Some(artifacts -> wrapperName) =>
            try {

              // call the function
              Some(function(artifacts))
            } catch {
              case e: Exception =>
                logger.error(
                  f"Failed to process ${artifact.path()} -- ${artifact.mimeType} wrapper ${wrapperName} exception ${e
                      .getMessage()}",
                  e
                )
                None
            }
        }
      } finally {
        // clean up
        Helpers.deleteDirectory(tempDir)

      }
    }
  }

  /** A stream of ArtifactWrappers... maybe
    */
  private type OptionalArchiveStream =
    Option[(Vector[ArtifactWrapper], String)]
}
