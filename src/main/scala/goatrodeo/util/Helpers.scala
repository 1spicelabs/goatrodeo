/* Copyright 2024 David Pollak, Spice Labs, Inc. & Contributors

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. */

package goatrodeo.util

import goatrodeo.omnibor.StringOrPair
import io.bullet.borer.Cbor
import org.apache.bcel.classfile.ClassParser
import org.apache.commons.compress.archivers.ArchiveEntry
import org.apache.commons.compress.archivers.ArchiveInputStream

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.IOException
import java.io.InputStream
import java.io.OutputStream
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import java.security.MessageDigest
import java.security.SecureRandom
import java.text.NumberFormat
import java.text.SimpleDateFormat
import java.util.Date
import java.util.TimeZone
import java.util.concurrent.atomic.AtomicReference
import scala.collection.immutable.TreeMap
import scala.collection.immutable.TreeSet
import scala.util.Try
type GitOID = String

/** A bunch of helpers/utilities
  */
object Helpers {

  /** Merge TreeMaps together
    *
    * @param a
    *   TreeMap
    * @param b
    *   TreeMap
    *
    * @return
    *   the merged TreeMap
    */
  def mergeTreeMaps(
      a: TreeMap[String, TreeSet[StringOrPair]],
      b: TreeMap[String, TreeSet[StringOrPair]]
  ): TreeMap[String, TreeSet[StringOrPair]] = {

    var ret = a
    for { (k, v) <- b } {
      val nv = ret.get(k) match {
        case None     => v
        case Some(mv) => v ++ mv
      }
      ret = ret + (k -> nv)
    }

    ret

  }

  /** The random number generator
    */
  private lazy val secRandom = new SecureRandom()

  /** Get a random int
    *
    * @return
    *   a random int
    */
  def randomInt(): Int = {
    secRandom.synchronized {
      while (true) {
        val ret = secRandom.nextInt();
        if (ret >= 0) return ret;
      }
      return 0;
    }
  }

  /** Get a random long
    *
    * @return
    *   random number
    */
  def randomLong(): Long = {
    secRandom.synchronized {
      while (true) {
        val ret = secRandom.nextLong()
        if (ret >= 0L) return ret;
      }
      return 0;
    }
  }

  /** Get a random byte array
    *
    * @param len
    *   the length of the array
    * @return
    *   the newly created random array
    */
  def randomBytes(len: Int): Array[Byte] = {
    secRandom.synchronized {
      val ret = new Array[Byte](len)
      secRandom.nextBytes(ret)
      ret
    }
  }

  /** Mime types for Java class files
    */
  lazy val javaClassMimeTypes: Set[String] = Set(
    "application/java-vm",
    "application/java-byte-code",
    "application/x-class-file",
    "application/x-java-class",
    "application/x-java-vm"
  )

  /** Given an input, see if the there is an associated source file. Currently
    * works on JVM `.class` files.
    *
    * @param file
    *   the file to test
    * @param mimeType
    *   the mime type of the file
    * @param associatedFiles
    *   the filename to gitoid relatationship for source or predicate files
    *
    * @return
    *   a set of GitOIDs for the source files
    */
  def computeAssociatedSource(
      file: ArtifactWrapper,
      associatedFiles: Map[String, GitOID]
  ): TreeSet[GitOID] = {
    file.mimeType match {
      case maybeClass if javaClassMimeTypes.contains(maybeClass) =>
        val sourceName: Option[String] =
          Try {
            val is = file.asStream()
            try {
              val cp = new ClassParser(is, file.path())

              val clz = cp.parse()
              clz.getSourceFilePath()
            } catch {
              case e: OutOfMemoryError =>
                // if the classfile is corrupt, we may get an OOME, swallow it and just don't
                // return a file name
                throw new Exception(f"Failed to parse class ${e.getMessage()}")
            } finally {
              is.close()
            }
          }.toOption

        val sourceGitOID = for {
          sn <- sourceName
          pf <- associatedFiles.get(sn)
        } yield {
          pf
        }

        sourceGitOID match {
          case None    => TreeSet()
          case Some(s) => TreeSet(s)
        }

      case _ => TreeSet()
    }

  }

  /** Get the current date in ISO 8601 format
    */
  def currentDate8601(): String = {
    val timezone = TimeZone.getTimeZone("UTC")
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
    dateFormat.setTimeZone(timezone);
    dateFormat.format(new Date())
  }

  /** Given a file root and a filter function, return a channel that contains
    * the files found in the folder and subfolders that match the filter.
    *
    * @param root
    *   the root directory to search
    * @param ok
    *   the filter function
    * @return
    *   the found files
    */
  def findFiles(
      root: File,
      ok: File => Boolean
  ): Vector[File] = {

    if (root.isDirectory()) {
      Option(root.listFiles()).toVector
        .flatMap(_.toVector)
        .flatMap(findFiles(_, ok))
    } else if (root.isFile() && ok(root) && !root.getName().startsWith(".")) {
      Vector(root)
    } else Vector()
  }

  /** Write data over a file
    *
    * @param what
    *   the file to write
    * @param data
    *   the data
    */
  def writeOverFile(what: File, data: String): Unit = {
    writeOverFile(what, data.getBytes("UTF-8"))
  }

  /** Write data over a file
    *
    * @param what
    *   the file to write
    * @param data
    *   the data
    */
  def writeOverFile(what: File, data: Array[Byte]): Unit = {
    val fos = new FileOutputStream(what, false)
    fos.write(data)
    fos.close()
  }

  /** Compute the MD5 hash of a String (converted to bytes using UTF-8
    * encoding). Note MD5 is faster and more space efficient than secure hashes.
    * It's used to compute the hash of file paths/names for indexing.
    *
    * @param in
    *   the String to get the hash for
    * @return
    *   the hex of the MD5 hash.
    */
  def md5hashHex(in: String): String = {

    toHex(computeMD5(stringToInputStream(in)))
  }

  def stringToInputStream(str: String): InputStream = {
    new ByteArrayInputStream(str.getBytes("UTF-8"))
  }

  /** Compute the MD5 hash of an input stream. Note MD5 is faster and more space
    * efficient than secure hashes. It's used to compute the hash of file
    * paths/names for indexing.
    *
    * @param in
    *   the String to get the hash for
    * @return
    *   the 16 bytes of the MD5 hash
    */
  def computeMD5(in: InputStream): Array[Byte] = {
    val md = MessageDigest.getInstance("MD5")
    val ba = new Array[Byte](4096)
    while (true) {
      val len = in.read(ba)
      if (len <= 0) {
        in.close()
        return md.digest()
      }

      md.update(ba, 0, len)
    }
    ???
  }

  /** Compute the MD5 hash of an input stream. Note MD5 is faster and more space
    * efficient than secure hashes. It's used to compute the hash of file
    * paths/names for indexing.
    *
    * @param in
    *   the String to get the hash for
    * @return
    *   the 16 bytes of the MD5 hash
    */
  def computeMD5(in: String): Array[Byte] = {
    computeMD5(stringToInputStream(in))
  }

  /** Compute the MD5 hash of an input stream. Note MD5 is faster and more space
    * efficient than secure hashes. It's used to compute the hash of file
    * paths/names for indexing.
    *
    * @param in
    *   the String to get the hash for
    * @return
    *   the 16 bytes of the MD5 hash
    */
  def computeMD5(in: File): Array[Byte] = {
    computeMD5(FileInputStream(in))
  }

  /** Compute the SHA1 hash of an input stream.
    *
    * @param in
    *   the String to get the hash for
    * @return
    *   the sha1 hash
    */
  def computeSHA1(in: String): Array[Byte] = {
    computeSHA1(stringToInputStream(in))
  }

  /** Compute the SHA1 hash of an input stream.
    *
    * @param in
    *   the String to get the hash for
    * @return
    *   the sha1 hash
    */
  def computeSHA1(in: File): Array[Byte] = {
    computeSHA1(FileInputStream(in))
  }

  /** Compute the SHA256 hash of a String
    *
    * @param in
    *   the String to get the hash for
    * @return
    *   the SHA256 hash
    */
  def computeSHA256(in: String): Array[Byte] = {
    computeSHA256(stringToInputStream(in))
  }

  /** Compute the SHA512 hash of a file.
    *
    * @param in
    *   the String to get the hash for
    * @return
    *   the sha512 hash
    */
  def computeSHA512(in: File): Array[Byte] = {
    computeSHA512(FileInputStream(in))
  }

  /** Compute the SHA512 hash of a String
    *
    * @param in
    *   the String to get the hash for
    * @return
    *   the SHA512 hash
    */
  def computeSHA512(in: String): Array[Byte] = {
    computeSHA512(stringToInputStream(in))
  }

  /** Compute the SHA256 hash of a file.
    *
    * @param in
    *   the String to get the hash for
    * @return
    *   the sha256 hash
    */
  def computeSHA256(in: File): Array[Byte] = {
    computeSHA256(FileInputStream(in))
  }

  /** Compute the SHA1 hash of an input stream.
    *
    * @param in
    *   the String to get the hash for
    * @return
    *   the SHA1 hash
    */
  def computeSHA1(in: InputStream): Array[Byte] = {
    val md = MessageDigest.getInstance("SHA1")
    val ba = new Array[Byte](4096)
    while (true) {
      val len = in.read(ba)
      if (len <= 0) {
        in.close()
        return md.digest()
      }

      md.update(ba, 0, len)
    }
    ???
  }

  /** Compute the sha512 of an input stream
    *
    * @param in
    *   the input stream
    * @return
    *   the bytes of the sha256 hash
    */
  def computeSHA512(in: InputStream): Array[Byte] = {
    val md = MessageDigest.getInstance("SHA512")
    val ba = new Array[Byte](4096)
    while (true) {
      val len = in.read(ba)
      if (len <= 0) {
        in.close()
        return md.digest()
      }

      md.update(ba, 0, len)
    }
    ???
  }

  /** Compute the sha256 of an input stream
    *
    * @param in
    *   the input stream
    * @return
    *   the bytes of the sha256 hash
    */
  def computeSHA256(in: InputStream): Array[Byte] = {
    val md = MessageDigest.getInstance("SHA256")
    val ba = new Array[Byte](4096)
    while (true) {
      val len = in.read(ba)
      if (len <= 0) {
        in.close()
        return md.digest()
      }

      md.update(ba, 0, len)
    }
    ???
  }

  /** Create a Scala `Iterator` for the `ArchiveInputStream`
    *
    * @param archive
    *   the ArchiveInputStream to iterate over
    * @return
    *   an iterator
    */
  def iteratorFor(
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

  /** Recursively delete a directory
    */
  def deleteDirectory(theDir: Path): Unit = {
    Files.walkFileTree(
      theDir,
      new SimpleFileVisitor[Path]() {
        override def visitFile(
            file: Path,
            attrs: BasicFileAttributes
        ): FileVisitResult = {
          Files.delete(file)
          FileVisitResult.CONTINUE
        }

        override def postVisitDirectory(
            x: Path,
            ioe: IOException
        ): FileVisitResult = {
          Files.delete(x)
          FileVisitResult.CONTINUE
        }

      }
    )
  }

  /** Takes a String. If the string contains a ':', the hex to binary conversion
    * starts on the character after the last ':'. Treats the rest of the String
    * a hex. For each hex pair, convert into a byte and append to `out`
    *
    * @param in
    *   the String to convert to binary
    * @param the
    *   output stream to append the bytes to
    */
  def convertHexToBinaryAndAppendToStream(
      in: String,
      out: OutputStream
  ): Unit = {
    val len = in.length()
    val lastPos = len - 2
    var pos = in.lastIndexOf(":") match {
      case -1 => 0
      case x  => x + 1
    }

    while (pos <= lastPos) {
      val hi = charToBin(in.charAt(pos))
      val low = charToBin(in.charAt(pos + 1))
      val byte = hi * 16 + low
      out.write(byte)
      pos += 2
    }
  }

  @inline def charToBin(c: Char): Int = {
    c match {
      case '0'       => 0
      case '1'       => 1
      case '2'       => 2
      case '3'       => 3
      case '4'       => 4
      case '5'       => 5
      case '6'       => 6
      case '7'       => 7
      case '8'       => 8
      case '9'       => 9
      case 'a' | 'A' => 10
      case 'b' | 'B' => 11
      case 'c' | 'C' => 12
      case 'd' | 'D' => 13
      case 'e' | 'E' => 14
      case 'f' | 'F' => 15
      case _         => 0
    }
  }

  @inline def hexChar(b: Byte): Char = {
    b match {
      case 0  => '0'
      case 1  => '1'
      case 2  => '2'
      case 3  => '3'
      case 4  => '4'
      case 5  => '5'
      case 6  => '6'
      case 7  => '7'
      case 8  => '8'
      case 9  => '9'
      case 10 => 'a'
      case 11 => 'b'
      case 12 => 'c'
      case 13 => 'd'
      case 14 => 'e'
      case 15 => 'f'
    }
  }

  /** Given a byte array, create a lowercase hexadecimal string representing the
    * array
    *
    * @param bytes
    *   the array of bytes
    * @return
    *   the hexadecimal representation of the bytes
    */
  def toHex(bytes: Array[Byte]): String = {
    val len = bytes.length
    val sb = new StringBuilder(len * 2)
    var cur = 0
    while (cur < len) {
      val b = bytes(cur)
      sb.append(hexChar(((b >> 4) & 0xf).toByte))
      sb.append(hexChar((b & 0xf).toByte))
      cur += 1
    }
    sb.toString()
  }

  def toHex(id: Long): String = {
    val bb = ByteBuffer.allocate(8)
    toHex(bb.putLong(id).position(0).array())
  }

  def byteArrayToLong63Bits(bytes: Array[Byte]): Long = {
    val toDo = new Array[Byte](8)

    System.arraycopy(bytes, 0, toDo, 0, Math.min(8, bytes.length))
    toDo(0) = (toDo(0) & 0x7f).toByte
    val byteBuffer = ByteBuffer.wrap(toDo)
    byteBuffer.position(0).getLong()
  }

  /** Slurp the first block (4K) from the File. If the File has less than 4K
    * bytes, returns all the readable bytes
    *
    * @param in
    * @return
    */
  def slurpBlock(in: File): Array[Byte] = {
    slurpBlock(FileInputStream(in))
  }

  /** Slurp the first block (4K) from the InputStream. If the InputStream has
    * less than 4K bytes, returns all the readable bytes
    *
    * @param in
    * @return
    */
  def slurpBlock(in: InputStream): Array[Byte] = {
    val ret = new ByteArrayOutputStream()
    val buffer = new Array[Byte](4096)

    val len = in.read(buffer)
    in.close()
    if (len == 0) {
      Array()
    } else {
      ret.write(buffer, 0, len)
      ret.toByteArray()
    }
  }

  /** Slurp the contents of an InputStream
    *
    * @param what
    *   the InputStream
    * @return
    *   the bytes contained in the InputStream
    */
  def slurpInput(what: InputStream): Array[Byte] = {
    val ret = new ByteArrayOutputStream()
    val buffer = new Array[Byte](4096)

    while (true) {
      val len = what.read(buffer)
      if (len < 0) {
        what.close()
        return ret.toByteArray()
      }
      if (len > 0) {
        ret.write(buffer, 0, len)
      }
    }

    what.close()
    ret.toByteArray()
  }

  /** Copy from input stream to output stream
    *
    * @param in
    *   the input stream
    * @param out
    *   the output stream
    * @return
    *   number of bytes
    */
  def copy(in: InputStream, out: OutputStream): Long = {
    var cnt = 0L
    val buffer: Array[Byte] = new Array[Byte](4096)
    var bytesRead = 0
    while ({
      bytesRead = in.read(buffer)
      bytesRead >= 0
    }) {
      if (bytesRead > 0) {
        cnt += bytesRead
        out.write(buffer, 0, bytesRead)
      }
    }
    out.flush()
    cnt
  }

  def slurpInputToString(what: InputStream): String = {
    new String(slurpInput(what), "UTF-8")
  }

  /** Slurp the contents of an InputStream
    *
    * @param what
    *   the InputStream
    * @return
    *   the bytes contained in the InputStream
    */
  def slurpInputNoClose(what: InputStream): Array[Byte] = {
    val ret = new ByteArrayOutputStream()
    val buffer = new Array[Byte](4096)

    while (true) {
      val len = what.read(buffer)
      if (len < 0) {

        return ret.toByteArray()
      }
      if (len > 0) {
        ret.write(buffer, 0, len)
      }
    }

    ret.toByteArray()
  }

  /** Slurp the contents of an InputStream into a path
    *
    * @param what
    *   the InputStream
    * @param close_?
    *   close the stream after reading
    * @param dest
    *   the path to put the bytes in
    */
  def streamToFile(
      what: InputStream,
      close_? : Boolean,
      dest: File
  ): Unit = {
    val ret = FileOutputStream(dest)
    val buffer = new Array[Byte](4096)

    while (true) {
      val len = what.read(buffer)
      if (len < 0) {
        if (close_?) {
          what.close()
        }
        ret.close()
        return
      }
      if (len > 0) {
        ret.write(buffer, 0, len)
      }
    }
    ret.close()

  }

  /** Slurp the contents of an InputStream into a temp file
    *
    * @param what
    *   the InputStream
    * @return
    *   a file that contains the contents of the stream
    */
  def tempFileFromStream(
      what: InputStream,
      close_? : Boolean,
      tempDir: Path
  ): File = {

    val retFile = Files.createTempFile(tempDir, "goats", ".temp").toFile()
    val ret = FileOutputStream(retFile)
    val buffer = new Array[Byte](4096)

    while (true) {
      val len = what.read(buffer)
      if (len < 0) {
        if (close_?) {
          what.close()
        }
        ret.close()
        return retFile
      }
      if (len > 0) {
        ret.write(buffer, 0, len)
      }
    }
    ret.close()
    retFile
  }

  /** Bail out... gracefully if we're running in SBT
    *
    * @return
    */
  def bailFail(): Nothing = {
    if (Thread.currentThread().getStackTrace().length < 6) System.exit(1)
    throw new Exception()
  }

  def readLenAndCBOR[A](
      fc: FileChannel
  )(implicit decoder: io.bullet.borer.Decoder[A]): A = {
    val len = Helpers.readInt(fc)
    readCBOR(fc, len)
  }

  def readCBOR[A](fc: FileChannel, len: Int)(implicit
      decoder: io.bullet.borer.Decoder[A]
  ): A = {

    val dest = ByteBuffer.allocate(len)
    val bytesRead = fc.read(dest)
    if (bytesRead != len) {
      throw Exception(f"Trying to read ${len} bytes but only got ${bytesRead}")
    }
    Cbor.decode(dest).to[A].value
  }

  /** Slurp the contents of a File
    *
    * @param what
    *   the File
    * @return
    *   the bytes contained in the File
    */
  def slurpInput(what: File): Array[Byte] = {
    val fis = new FileInputStream(what)
    slurpInput(fis)
  }

  private val allFiles: AtomicReference[Map[String, TreeSet[String]]] =
    new AtomicReference(Map())

  def filesForParent(in: File): TreeSet[String] = {
    val parentFile = in.getAbsoluteFile().getParentFile()
    val parentStr = parentFile.getAbsolutePath()

    allFiles.get().get(parentStr) match {
      case Some(r) => r
      case None =>
        val v = TreeSet(parentFile.listFiles().map(f => f.getName())*)
        allFiles.getAndUpdate(last => last + (parentStr -> v))
        v
    }
  }

  def formatInt(in: Int): String = {
    NumberFormat.getInstance().format(in)

  }

  def findAllFiles(root: File, start: Vector[File] = Vector()): Vector[File] = {
    var base = start
    if (root.isDirectory()) {
      val fileList = root.listFiles()
      if (fileList != null) {
        for { kid <- fileList } {
          base = findAllFiles(kid, base)
        }
        base
      } else base
    } else if (root.isFile()) {
      base :+ root

    } else base
  }

  def formatInt(in: Long): String = {
    NumberFormat.getInstance().format(in)
  }

  def findSrcFile(like: File): Option[File] = {
    val name = like.getName()
    val myNameIsh = name.substring(0, name.length() - 4)
    val possible = filesForParent(like)
    val ns = f"${myNameIsh}-sources.jar"

    possible.contains(ns) match {
      case true =>
        val maybe = new File(like.getAbsoluteFile().getParentFile(), ns)
        if (maybe.exists()) { Some(maybe) }
        else { None }
      case _ =>
        None
    }

  }

  def readShort(reader: FileChannel): Int = {
    val bytes = ByteBuffer.allocate(2)
    reader.read(bytes)

    bytes.position(0).getShort().toInt & 0xffff
  }

  def readInt(reader: FileChannel): Int = {
    val bytes = ByteBuffer.allocate(4)
    reader.read(bytes)

    bytes.position(0).getInt()
  }

  def readLong(reader: FileChannel): Long = {
    val bytes = ByteBuffer.allocate(8) // new Array[Byte](8)
    reader.read(bytes)
    val byteBuffer = bytes
    byteBuffer.position(0).getLong()
  }

  def writeShort(writer: FileChannel, num: Int): Unit = {
    val bytes = ByteBuffer.allocate(2)
    bytes.putShort((num & 0xffff).toShort).flip()
    val len = writer.write(bytes)
  }

  def writeInt(writer: FileChannel, num: Int): Unit = {
    val bytes = ByteBuffer.allocate(4).putInt(num).flip()
    writer.write(bytes)
  }

  def writeLong(writer: FileChannel, num: Long): Unit = {
    val bytes = ByteBuffer.allocate(8).putLong(num).flip()
    writer.write(bytes)
  }
}

/** A set of helpers to manage GitOIDs
  */
object GitOIDUtils {

  /** Given a full OmniBOR URI, parse into a triple of first 3 hex chars of
    * hash, second 3 hex chars of hash, rest of the hex chars of hash
    *
    * @param uri
    *   the OmniBOR URI
    * @return
    *   the split filename
    */
  def urlToFileName(uri: String): (String, String, String) = {
    val idx = uri.lastIndexOf(":")
    val str = if (idx >= 0) {
      uri.substring(idx + 1)
    } else uri
    (str.substring(0, 3), str.substring(3, 6), str.substring(6))
  }

  /** The object type
    */
  enum ObjectType {
    case Blob, Tree, Commit, Tag

    /** Get the canonical name for the Object Type
      *
      * @return
      *   the canonical name for the object type
      */
    def gitoidName(): String = {
      this match {
        case Blob   => "blob"
        case Tree   => "tree"
        case Commit => "commit"
        case Tag    => "tag"
      }
    }
  }

  /** Takes a set of gitoids, sorts them, converts to binary, and generates the
    * Gitoid tree
    *
    * @param gitoids
    *   the gitoids to build the tree for
    *
    * @return
    *   the computed tree
    */
  def merkleTreeFromGitoids(
      gitoids: Vector[String],
      hashType: HashType = HashType.SHA256
  ): String = {
    val sorted = gitoids.sorted
    val out = new ByteArrayOutputStream()
    for (gitoid <- sorted) {
      Helpers.convertHexToBinaryAndAppendToStream(gitoid, out)
    }
    out.flush()
    val ba = out.toByteArray()
    val in = new ByteArrayInputStream(ba)
    url(in, ba.length, hashType, ObjectType.Tree)
  }

  /** The hash type
    */
  enum HashType {
    case SHA1, SHA256

    /** Based on the hash type, get the MessageDigest
      *
      * @return
      *   the MessageDigest for the type
      */
    def getDigest(): MessageDigest = {
      this match {
        case SHA1   => MessageDigest.getInstance("SHA-1")
        case SHA256 => MessageDigest.getInstance("SHA-256")
      }
    }

    /** Get the canonical name for the hash type
      *
      * @return
      *   the canonical name
      */
    def hashTypeName(): String = {
      this match {
        case SHA1   => "sha1"
        case SHA256 => "sha256"
      }
    }
  }

  /** Given an object type, a pile of bytes, and a hash type, compute the GitOID
    *
    * @param hashType
    *   the hash type
    * @param type
    *   the type of object
    * @param bytes
    *   the bytes to compute the gitoid for
    * @return
    *   the gitoid
    */
  def computeGitOID(
      bytes: InputStream,
      len: Long,
      hashType: HashType = HashType.SHA256,
      tpe: ObjectType = ObjectType.Blob
  ): Array[Byte] = {
    // get the prefix bytes in local encoding... which should be okay given that
    // the string should be ASCII
    val prefix =
      String.format("%s %d\u0000", tpe.gitoidName(), len).getBytes();
    val md = hashType.getDigest();
    md.update(prefix);

    val buf = new Array[Byte](4096)
    var keepRunning = true
    while (keepRunning) {
      val read = bytes.read(buf)
      if (read <= 0) {
        bytes.close()
        keepRunning = false
      } else {
        md.update(buf, 0, read)
      }
    }

    md.digest()
  }

  /** Take bytes, compute the GitOID and return the hexadecimal bytes
    * representing the GitOID
    *
    * @param bytes
    *   the bytes to compute gitoid for
    * @param hashType
    *   the hash type... default SHA256
    * @param tpe
    *   the object type... default Blob
    * @return
    *   the hex representation of the GitOID
    */
  def hashAsHex(
      bytes: InputStream,
      len: Long,
      hashType: HashType = HashType.SHA256,
      tpe: ObjectType = ObjectType.Blob
  ): String = {
    Helpers.toHex(
      computeGitOID(bytes, len, hashType, tpe)
    )
  }

  /** A `gitoid` URL. See
    * https://www.iana.org/assignments/uri-schemes/prov/gitoid
    *
    * @return
    *   the `gitoid` URL
    */
  def url(
      inputStream: InputStream,
      len: Long,
      hashType: HashType,
      tpe: ObjectType = ObjectType.Blob
  ): String = {
    String.format(
      "gitoid:%s:%s:%s",
      tpe.gitoidName(),
      hashType.hashTypeName(),
      hashAsHex(inputStream, len, hashType, tpe)
    ) /*.intern()*/
  }

  def computeAllHashes(
      theFile: ArtifactWrapper
  ): (String, Vector[String]) = {
    def is(): InputStream = theFile.asStream()
    val gitoidSha256 = url(is(), theFile.size(), HashType.SHA256)

    (
      gitoidSha256,
      Vector(
        url(is(), theFile.size(), HashType.SHA1),
        String
          .format(
            "sha1:%s",
            Helpers.toHex(Helpers.computeSHA1(is()))
          ) /*.intern()*/,
        String
          .format(
            "sha256:%s",
            Helpers.toHex(Helpers.computeSHA256(is()))
          ) /*.intern()*/,
        String
          .format(
            "sha512:%s",
            Helpers.toHex(Helpers.computeSHA512(is()))
          ) /*.intern()*/,
        String
          .format(
            "md5:%s",
            Helpers.toHex(Helpers.computeMD5(is()))
          ) /*.intern()*/
      )
    )
  }
}
