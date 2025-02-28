package goatrodeo.omnibor

import com.typesafe.scalalogging.Logger
import goatrodeo.envelopes.ClusterFileEnvelope
import goatrodeo.envelopes.DataFileEnvelope
import goatrodeo.envelopes.IndexFileEnvelope
import goatrodeo.envelopes.Position
import goatrodeo.util.Helpers

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.Files
import java.time.Duration
import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset
import scala.math.Ordering.Implicits._
import scala.util.Try

/** Manage many parts of persisting/retrieving the graph information
  */
object GraphManager {
  val logger: Logger = Logger(getClass())
  object Consts {
    val DataFileMagicNumber: Int = 0x00be1100 // Bell
    val IndexFileMagicNumber: Int = 0x54154170 // Shishitō
    val ClusterFileMagicNumber: Int = 0xba4a4a // Banana
    val TargetMaxFileSize: Long = 15L * 1024L * 1024L * 1024L // 15G
  }

  case class DataAndIndexFiles(dataFile: Long, indexFile: Long)
  private def writeABlock(
      targetDirectory: File,
      items: Iterator[Item],
      previous: Long,
      afterWrite: Item => Unit
  ): DataAndIndexFiles = {
    val start = Instant.now()
    // create temporary file
    val tempFile =
      Files.createTempFile(targetDirectory.toPath(), "goat_rodeo_data_", ".grd")

    val fileWriter = new FileOutputStream(tempFile.toFile())
    val writer = fileWriter.getChannel()
    var previousPosition: Long = 0
    Helpers.writeInt(writer, Consts.DataFileMagicNumber)
    val dataFileEnvelope =
      DataFileEnvelope.build(
        previous = previous,
        builtFromMerge = false
      )
    val envelopeBytes = dataFileEnvelope.encode()
    // write the DataFileEnvelope length
    Helpers.writeShort(writer, envelopeBytes.length)

    // write DataFileEnvelope
    val writtenLen = writer.write(ByteBuffer.wrap(envelopeBytes))
    var loopCnt = 0

    var pairs: Vector[(String, Array[Byte], Position)] = Vector()

    // loop writing entries until empty or the file is >= 16GB in size
    while (items.hasNext && writer.position() < Consts.TargetMaxFileSize) {
      val orgEntry = items.next()
      val currentPosition = writer.position()
      // val entry = orgEntry.fixReferencePosition(0L, currentPosition)
      val entry = orgEntry

      val md5 = entry.identifierMD5()

      val entryBytes =
        entry.encodeCBOR() // compression.compress(entry.encodeCBOR())

      pairs = pairs.appended((Helpers.toHex(md5), md5, currentPosition))

      val toAlloc = 256 + (entryBytes.length)
      val bb = ByteBuffer.allocate(toAlloc)

      bb.putInt(entryBytes.length)
      bb.put(entryBytes)

      bb.flip()

      writer.write(bb)

      previousPosition = currentPosition; // itemEnvelope.position;

      afterWrite(entry)
      loopCnt += 1
      if (loopCnt % 1000000 == 0) {
        logger.info(
          f"Write loop ${loopCnt} at ${Duration.between(start, Instant.now())}"
        )
      }
    }

    Helpers.writeShort(writer, -1) // a marker that says end of file

    // write final back-pointer (to the last entry record)
    Helpers.writeLong(writer, previousPosition)

    // compute SHA256 of the file
    writer.close()

    logger.info(
      f"Finished write loop at ${Duration.between(start, Instant.now())}"
    )

    // rename the file to <sha256>.grd
    val sha256Long = Helpers.byteArrayToLong63Bits(
      Helpers.computeSHA256(new FileInputStream(tempFile.toFile()))
    )

    val targetFileName =
      new File(targetDirectory, f"${Helpers.toHex(sha256Long)}.grd")

    tempFile.toFile().renameTo(targetFileName)

    logger.info(f"Finished rename at ${Duration.between(start, Instant.now())}")

    val targetIndexName =
      new File(targetDirectory, f"${Helpers.toHex(sha256Long)}.gri")

    val indexWriter = new FileOutputStream(targetIndexName).getChannel()
    Helpers.writeInt(indexWriter, Consts.IndexFileMagicNumber)
    val indexEnv = IndexFileEnvelope.build(
      size = pairs.length,
      dataFiles = Vector(sha256Long)
    )
    val indexEnvBytes = indexEnv.encode()
    Helpers.writeShort(indexWriter, indexEnvBytes.length)

    indexWriter.write(ByteBuffer.wrap(indexEnvBytes))
    val indexBB = ByteBuffer.allocate(pairs.length * 32)

    for { v <- pairs } {
      indexBB.put(v._2)
      indexBB.putLong(sha256Long)
      indexBB.putLong(v._3)
    }

    indexBB.flip()

    indexWriter.write(indexBB)

    indexWriter.close()

    logger.info(
      f"Finished index write at ${Duration.between(start, Instant.now())}"
    )

    val indexSha256Long = Helpers.byteArrayToLong63Bits(
      Helpers.computeSHA256(new FileInputStream(targetIndexName))
    )

    val indexTargetFileName =
      new File(targetDirectory, f"${Helpers.toHex(indexSha256Long)}.gri")

    targetIndexName.renameTo(indexTargetFileName)

    logger.info(
      f"Finished index rename at ${Duration.between(start, Instant.now())}"
    )

    DataAndIndexFiles(sha256Long, indexSha256Long)
  }

  def writeEntries(
      targetDirectory: File,
      entries: Iterator[Item]
  ): (Seq[DataAndIndexFiles], File) = {
    var previousInChain: Long = 0L
    var biggest: Vector[(Item, Int)] = Vector()

    def updateBiggest(item: Item): Unit = {
      val containedBy =
        item.connections.filter(_._1 == EdgeType.containedBy).size
      if (biggest.length <= 50) {
        biggest = (biggest :+ (item -> containedBy)).sortBy(_._2).reverse
      } else if (biggest.last._2 < containedBy) {
        biggest =
          (biggest.dropRight(1) :+ (item -> containedBy)).sortBy(_._2).reverse
      }
    }

    var fileSet: List[DataAndIndexFiles] = Nil
    while (entries.hasNext) {
      val dataAndIndex = writeABlock(
        targetDirectory,
        entries,
        previous = previousInChain,
        updateBiggest
      )
      previousInChain = dataAndIndex.dataFile
      fileSet = dataAndIndex :: fileSet

    }

    val tempFile =
      Files.createTempFile(
        targetDirectory.toPath(),
        "goat_rodeo_cluster_",
        ".grc"
      )
    val fileWriter = new FileOutputStream(tempFile.toFile())
    val writer = fileWriter.getChannel()

    Helpers.writeInt(writer, Consts.ClusterFileMagicNumber)
    val clusterEnvelope =
      ClusterFileEnvelope.build(
        indexFiles = fileSet.map(_.indexFile).toVector,
        dataFiles = fileSet.map(_.dataFile).toVector
      )
    val envelopeBytes = clusterEnvelope.encode()
    Helpers.writeShort(writer, envelopeBytes.length)
    writer.write(ByteBuffer.wrap(envelopeBytes))
    writer.close()
    val sha256Long = Helpers.byteArrayToLong63Bits(
      Helpers.computeSHA256(new FileInputStream(tempFile.toFile()))
    )

    val now = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC)

    val targetFile =
      new File(
        targetDirectory,
        f"${now.getYear()}_${"%02d".format(now.getMonthValue())}_${"%02d"
            .format(now.getDayOfMonth())}_${"%02d".format(
            now
              .getHour()
          )}_${"%02d".format(now.getMinute())}_${"%02d"
            .format(now.getSecond())}_${Helpers.toHex(sha256Long)}.grc"
      )

    tempFile.toFile().renameTo(targetFile)
    if (false) {
      for { i <- biggest } {
        logger.info(
          f"Item ${i._1.identifier} ${i._1.body.map(_.fileNames).getOrElse(Vector())} has ${i._2} connections"
        )
      }
    }
    (fileSet, targetFile)
  }

}

class GRDWalker(source: FileChannel) {
  def open(): Try[DataFileEnvelope] = {
    val magic_? = Helpers.readInt(source)
    if (magic_? != GraphManager.Consts.DataFileMagicNumber) {
      // FIXME log the error
      throw new Exception(f"Found incorrect magic number ${magic_?}")
    }

    val len = Helpers.readShort(source)
    val ba = ByteBuffer.allocate(len)
    val readLen = source.read(ba)
    if (len != readLen) {
      throw new Exception(f"Wanted ${len} bytes got ${readLen}")
    }
    DataFileEnvelope.decode(ba.position(0).array())
  }

  def readNext(): Option[Item] = {
    if (source.position() == source.size()) {
      None
    } else {
      val entryLen = Helpers.readInt(source)
      if (entryLen == -1) {
        None
      } else {
        val entryByteBuffer = ByteBuffer.allocate(entryLen)
        source.read(entryByteBuffer)

        val entryBytes = entryByteBuffer.array()
        val entry = Item.decode(entryBytes).get
        Some(entry)
      }
    }
  }

  def items(): Iterator[Item] = {
    var nextItem = readNext()
    new Iterator[Item] {

      override def hasNext: Boolean = nextItem.isDefined

      override def next(): Item = {
        val ret = nextItem
        nextItem = readNext()
        ret.get // should be save because tested in hasNext
      }

    }
  }
}
