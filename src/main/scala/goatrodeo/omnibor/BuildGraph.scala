package goatrodeo.omnibor

import java.io.File
import scala.util.Try
import org.apache.commons.compress.compressors.CompressorStreamFactory
import goatrodeo.util.Helpers
import java.io.BufferedInputStream
import java.io.FileInputStream
import org.apache.commons.compress.archivers.ArchiveInputStream
import org.apache.commons.compress.archivers.ArchiveEntry
import org.apache.commons.compress.archivers.ArchiveStreamFactory
import goatrodeo.util.PackageIdentifier
import goatrodeo.util.GitOID
import goatrodeo.util.GitOIDUtils
import goatrodeo.util.FileType
import java.io.IOException
import java.util.zip.ZipFile
import java.util.zip.ZipEntry
import java.io.InputStream
import java.io.ByteArrayInputStream

trait ArtifactWrapper {
  def asStream(): InputStream
  def name(): String
  def size(): Long
  def isFile(): Boolean
  def isDirectory(): Boolean
  def isRealFile(): Boolean
  //def asFile(): (File, Boolean)
  def listFiles(): Vector[ArtifactWrapper]
  def getCanonicalPath(): String
  def getParentDirectory(): File
  def delete(): Boolean
  def exists(): Boolean
}

case class FileWrapper(f: File) extends ArtifactWrapper {
  def exists(): Boolean = f.exists()
  override def isRealFile(): Boolean = true

  override def delete(): Boolean = f.delete()

  override def isFile(): Boolean = f.isFile()

  override def listFiles(): Vector[ArtifactWrapper] =
    f.listFiles().toVector.map(FileWrapper(_))

  override def getParentDirectory(): File = f.getAbsoluteFile().getParentFile()

  override def getCanonicalPath(): String = f.getCanonicalPath()

  // override def asFile(): (File, Boolean) = (f, false)

  override def isDirectory(): Boolean = f.isDirectory()

  override def asStream(): InputStream = BufferedInputStream(FileInputStream(f))

  override def name(): String = f.getName()

  override def size(): Long = f.length()
}

case class ByteWrapper(bytes: Array[Byte], fileName: String)
    extends ArtifactWrapper {

  def exists(): Boolean = true

  override def isRealFile(): Boolean = false

  override def delete(): Boolean = true

  override def isFile(): Boolean = true

  override def listFiles(): Vector[ArtifactWrapper] = Vector()

  override def getParentDirectory(): File = File("/")

  override def getCanonicalPath(): String = "/"

  // override def asFile(): (File, Boolean) =
  //   (Helpers.tempFileFromStream(asStream(), true, ".goat"), true)

  override def isDirectory(): Boolean = false

  override def asStream(): InputStream = ByteArrayInputStream(bytes)

  override def name(): String = fileName

  override def size(): Long = bytes.length
}

/** Tools for opening files including containing files and building graphs
  */
object BuildGraph {

  /** Look at a File. If it's compressed, read the full stream into a new file
    * and return that file
    *
    * @param in
    *   the file to inspect
    * @return
    *   a decompressed file
    */
  def fileForCompressed(in: InputStream): Option[File] = {
    val ret = Try {
      val fis = in
      try {
        new CompressorStreamFactory()
          .createCompressorInputStream(
            fis
          )
      } catch {
        case e: Exception => {
          fis.close()
          throw e
        }
      }
    }

    ret.toOption match {
      case Some(stream) =>
        Some(Helpers.tempFileFromStream(stream, true, ".goats"))
      case None => None
    }
  }

  /** Given a file that might be an archive (Zip, cpio, tar, etc.) or might be a
    * compressed archive (e.g. tar.Z), return a stream of `ArchiveEntry` so the
    * archive can be walked.
    *
    * @param in
    *   the file to test
    * @return
    *   an `ArchiveStream` if the file is an archive
    */
  def streamForArchive(
      in: ArtifactWrapper
  ): Option[(Iterator[() => (String, ArtifactWrapper)], () => Unit)] = {

    if (
      in
        .name()
        .endsWith(".zip") || in.name().endsWith(".jar") || in.name().endsWith(".war")
    ) {
      try {
        import scala.collection.JavaConverters.asScalaIteratorConverter
        val theFile = in match {
          case FileWrapper(f) => f
          case _ => Helpers.tempFileFromStream(in.asStream(), true, in.name())
        }
        val zipFile = ZipFile(theFile)
        val it: Iterator[() => (String, ArtifactWrapper)] = zipFile
          .stream()
          .iterator()
          .asScala
          .filter(v => {!v.isDirectory()})
          .map(v =>
            () => {
              val name = v.getName()

              val wrapper =
                if (name.endsWith(".zip") || name.endsWith(".jar") || name.endsWith(".war")) {
                  FileWrapper(
                    Helpers
                      .tempFileFromStream(zipFile.getInputStream(v), false, name)
                  )
                } else {
                  ByteWrapper(
                    Helpers.slurpInput(zipFile.getInputStream(v)),
                    name
                  )
                }

              (
                name,
                wrapper
              )
            }
          )
        return Some((it -> (() => { zipFile.close(); () })))
      } catch {
        case e: Exception => {} // fall through
      }
    }

    val factory = (new ArchiveStreamFactory())
    val ret = Try {
      {
        val fis = in.asStream()

        try {
          val input: ArchiveInputStream[ArchiveEntry] = factory
            .createArchiveInputStream(
              fis
            )
          val theIterator = iteratorFor(input)
            .filter(!_.isDirectory())
            .map(ae =>
              () => {
                val name = ae.getName()

                val wrapper =
                  if (name.endsWith(".zip") || name.endsWith(".jar")) {
                    FileWrapper(
                      Helpers
                        .tempFileFromStream(input, false, name)
                    )
                  } else {
                    ByteWrapper(Helpers.slurpInputNoClose(input), name)
                  }

                (name, wrapper)
              }
            )
          Some(theIterator -> (() => input.close()))
        } catch {
          case e: Throwable => fis.close(); None
        }
      }
    }.toOption.flatten

    ret match {
      case Some(archive) => Some(archive)
      case None => {
        for {
          uncompressedFile <- fileForCompressed(in.asStream())
          ret <- Try {

            val fis = FileInputStream(uncompressedFile)
            try {
              val input: ArchiveInputStream[ArchiveEntry] = factory
                .createArchiveInputStream(
                  BufferedInputStream(fis)
                )
              val theIterator = iteratorFor(input)
                .filter(!_.isDirectory())
                .map(ae =>
                  () => {
                    val name = ae.getName()

                    val wrapper =
                      if (name.endsWith(".zip") || name.endsWith(".jar")) {
                        FileWrapper(
                          Helpers
                            .tempFileFromStream(input, false, name)
                        )
                      } else {
                        ByteWrapper(Helpers.slurpInputNoClose(input), name)
                      }

                    (name, wrapper)
                  }
                )
              theIterator -> (() => {
                input.close(); uncompressedFile.delete(); ()
              })
            } catch {
              case e: Exception => fis.close(); throw e
            }
          }.toOption
        } yield ret
      }

    }
  }

  /** Create a Scala `Iterator` for the `ArchiveInputStream`
    *
    * @param archive
    *   the ArchiveInputStream to iterate over
    * @return
    *   an iterator
    */
  private def iteratorFor(
      archive: ArchiveInputStream[ArchiveEntry]
  ): Iterator[ArchiveEntry] = {
    new Iterator[ArchiveEntry] {
      var last: ArchiveEntry = null
      override def hasNext: Boolean = {
        last = archive.getNextEntry()
        last != null
      }

      override def next(): ArchiveEntry = last

    }
  }

  /** Process a file and subfiles (if the file is an archive)
    *
    * @param root
    *   the file to process
    * @param name
    *   the name of the file (not the name on disk, but the name based on where
    *   it came from, e.g. an archive)
    * @param parentId
    *   the optioned parent GitOID of the file if the file is contained within
    *   something else
    * @param action
    *   the action to take with the file and any of the subfiles (if this is an
    *   archive)
    */
  def processFileAndSubfiles(
      root: ArtifactWrapper,
      name: String,
      parentId: Option[String],
      action: (ArtifactWrapper, String, Option[String]) => String
  ): Unit = {
    val toProcess: Vector[ArtifactWrapper] = if (root.isFile()) { Vector(root) }
    else if (root.isDirectory()) { root.listFiles() }
    else Vector()

    for { workOn <- toProcess } {
      if (workOn.size() > 4) {
        val ret = action(
          workOn,
          if (workOn == root) name
          else
            workOn
              .getCanonicalPath()
              .substring(root.getParentDirectory().getCanonicalPath().length()),
          parentId
        )

        for {
          (stream, toDelete) <- streamForArchive(workOn)
        } {
          try {
            for {
              theFn <- stream
            } {
              val (name, file) = theFn()

              processFileAndSubfiles(file, name, Some(ret), action)
              file.delete()
            }

          } catch {
            case ioe: IOException if parentId.isDefined =>
            // println(
            //   f"Swallowed bad substream ${name} with exception ${ioe.getMessage()}"
            // )
          }

          // stream.close()
          toDelete()
        }
      }
    }
  }

  def graphForToProcess(item: ToProcess, store: Storage): Unit = {
    item match {
      case ToProcess(pom, main, Some(source), pomFile) => {
        // process the POM file
        pomFile.foreach(pf =>
          buildItemsFor(pf, pf.getName(), store, Vector(), None, Map())
        )

        // process the sources
        val sourceMap = buildItemsFor(
          source,
          pom.map(_.purl() + "?packaging=sources").getOrElse(main.getName()),
          store,
          Vector(),
          pom,
          Map()
        )

        // process the main class file
        buildItemsFor(
          main,
          pom.map(_.purl()).getOrElse(main.getName()),
          store,
          Vector(),
          pom,
          sourceMap
        )

      }

      case ToProcess(pom, main, _, _) =>
        buildItemsFor(
          main,
          pom.map(_.purl()).getOrElse(main.getName()),
          store,
          Vector(),
          pom,
          Map()
        )
    }
  }

  private def packageType(name: String): String = {
    name.indexOf("/") match {
      case n if n > 1 => name.substring(0, n)
      case _          => "pkg:"
    }
  }

  def buildItemsFor(
      root: File,
      name: String,
      store: Storage,
      topConnections: Vector[Edge],
      topPackageIdentifier: Option[PackageIdentifier],
      associatedFiles: Map[String, GitOID]
  ): Map[String, String] = {
    var ret: Map[String, String] = Map()
    processFileAndSubfiles(
      FileWrapper(root),
      name,
      None,
      (file, name, parent) => {
        val (main, foundAliases) = GitOIDUtils.computeAllHashes(file)

        val packageId = topPackageIdentifier
          .map(
            _.purl() +
              (if (
                 name.endsWith("?packaging=sources") ||
                 file.name().indexOf("-sources.") >= 0 ||
                 name.indexOf("-sources.") >= 0
               ) { "?packaging=sources" }
               else { "" })
          )
          .filter(_ => parent.isEmpty)
        val aliases = foundAliases ++ packageId.toVector

        val fileType = FileType.theType(name, Some(file), associatedFiles)

        val computedConnections: Set[Edge] =
          // built from a source file
          (fileType.sourceGitOid() match {
            case None => Set()
            case Some(source) =>
              Set[Edge]((source, EdgeType.BuiltFrom, None))
          })
          ++
          // include parent back-reference
          (parent match {
            case Some(parentId) =>
              Vector[Edge]((parentId, EdgeType.ContainedBy, None))
            case None => topConnections
          })
          ++
          // include aliases
          (
            aliases
              .map(alias => (alias, EdgeType.AliasFrom, None))
              .sortBy(_._1)
          )
          ++ // FIXME pURL DB?
          // create the pURL DB
          (
            packageId.toVector.map(id =>
              (packageType(id), EdgeType.ContainedBy, Some(id))
            )
          )

        val item = Item(
          identifier = main,
          reference = Item.noopLocationReference,
          connections = computedConnections,
          // altIdentifiers = aliases,
          previousReference = None,
          count = 1,
          metadata = Some(
            ItemMetaData.from(
              name,
              fileType,
              if (parent.isEmpty) topPackageIdentifier else None
            )
          ),
          mergedFrom = Vector(),
          _timestamp = System.currentTimeMillis(),
          _type = "Item",
          _version = 1
        ).fixReferences(store)
        ret = ret + (name -> main)

        store.write(
          main,
          current => {
            current match {
              case None        => item
              case Some(other) => other.merge(item)
            }
          }
        )
        main
      }
    )
    ret
  }
}
