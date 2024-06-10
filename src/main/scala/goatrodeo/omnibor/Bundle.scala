package goatrodeo.omnibor

import java.io.File
import java.math.BigInteger
import goatrodeo.util.Helpers
import scala.util.Using
import java.io.FileInputStream
import goatrodeo.envelopes.BundleFileEnvelope
import goatrodeo.omnibor.GraphManager.DataAndIndexFiles
import goatrodeo.envelopes.IndexFileEnvelope
import goatrodeo.envelopes.DataFileEnvelope
import java.nio.channels.FileChannel
import goatrodeo.envelopes.ItemEnvelope

case class IndexFile(
    envelope: IndexFileEnvelope,
    file: File,
    dataOffset: Long
) {
  def readIndex(): Vector[ItemOffset] = {
    ??? // FIXME write readIndex
    // let mut last = [0u8; 16];
    // let mut not_sorted = false;
    // let mut my_file = self
    //     .file
    //     .lock()
    //     .map_err(|e| anyhow!("Failed to lock {:?}", e))?;
    // let fp: &mut File = &mut my_file;
    // fp.seek(SeekFrom::Start(self.data_offset))?;

    // for _ in 0..self.envelope.size {
    //     let eo = EntryOffset::read(fp)?;
    //     if eo.hash < last {
    //         not_sorted = true;
    //     }
    //     last = eo.hash;
    //     ret.push(eo);
    // }

    // if not_sorted {
    //     ret.sort_by(|a, b| a.hash.cmp(&b.hash))
    // }

    // Ok(ret)
  }
}

object IndexFile {
  def open(dir: File, hash: Long): IndexFile = {
    val file = GoatRodeoBundle.findFile(dir, hash, "gri");
    val testedHash = Helpers.byteArrayToLong63Bits(Helpers.computeSHA256(file));
    if (testedHash != hash) {
      throw Exception(
        f"Index file for ${file.getName()} does not match ${String.format("%016x", testedHash)}"
      )
    }

    Using.resource(FileInputStream(file)) { inputStream =>
      val ipf = inputStream.getChannel()
      val magic = Helpers.readInt(ipf)
      if (magic != GraphManager.Consts.IndexFileMagicNumber) {
        throw Exception(
          f"Unexpected magic number ${magic} expecting ${GraphManager.Consts.IndexFileMagicNumber} for ${file.getName()}"
        )
      }

      val indexEnv: IndexFileEnvelope = Helpers.readLenAndCBOR(ipf)
      val indexPos = ipf.position()
      IndexFile(indexEnv, file, indexPos)
    }
  }

}

case class DataFile(
    envelope: DataFileEnvelope,
    private val file: FileChannel,
    dataOffset: Long
) {
  def readEnvelopeAt(pos: Long): ItemEnvelope = this.synchronized {
    file.position(pos)
    val len = Helpers.readShort(file)
    Helpers.readInt(file)
    Helpers.readCBOR(file, len)
  }

  def readEnvelopeAndItemAt(pos: Long): (ItemEnvelope, Item) =
    this.synchronized {
      file.position(pos)
      val len = Helpers.readShort(file)
      val itemLen = Helpers.readInt(file)
      val env: ItemEnvelope = Helpers.readCBOR(file, len)
      val item: Item = Helpers.readCBOR[Item](file, itemLen)

      (env -> item)
    }
}

object DataFile {
  def open(dir: File, hash: Long): DataFile = {
    val file = GoatRodeoBundle.findFile(dir, hash, "grd");
    val testedHash = Helpers.byteArrayToLong63Bits(Helpers.computeSHA256(file));
    if (testedHash != hash) {
      throw Exception(
        f"Index file for ${file.getName()} does not match ${String.format("%016x", testedHash)}"
      )
    }

    Using.resource(FileInputStream(file)) { inputStream =>
      val ipf = inputStream.getChannel()
      val magic = Helpers.readInt(ipf)
      if (magic != GraphManager.Consts.DataFileMagicNumber) {
        throw Exception(
          f"Unexpected magic number ${magic} expecting ${GraphManager.Consts.DataFileMagicNumber} for ${file.getName()}"
        )
      }

      val dataEnv: DataFileEnvelope = Helpers.readLenAndCBOR(ipf)
      val indexPos = ipf.position()
      DataFile(dataEnv, FileInputStream(file).getChannel(), indexPos)
    }
  }

}

case class GoatRodeoBundle(
    envelope: BundleFileEnvelope,
    path: File,
    dataFiles: Map[Long, DataFile],
    indexFiles: Map[Long, IndexFile]
)

object GoatRodeoBundle {
  def findFile(dir: File, hash: Long, suffix: String): File = {
    File(dir, f"${String.format("%016x", hash)}.${suffix}")
  }
  def open(path: File): GoatRodeoBundle = {

    val fileName = path.getName()
    val fileNameLen = fileName.length()
    val hash = BigInteger
      .apply(fileName.substring(fileNameLen - 20, fileNameLen - 4), 16)
      .longValue()

    val testedHash = Helpers.byteArrayToLong63Bits(Helpers.computeSHA256(path))

    if (testedHash != hash) {
      throw Exception(
        f"Bundle file file for '${fileName}' does not match actual hash ${String
            .format("%016x", testedHash)}"
      );
    }
    val theDir = path.getAbsoluteFile().getParentFile()

    Using.resource(FileInputStream(path)) { bundleFile =>
      val dfp = bundleFile.getChannel()
      val magic = Helpers.readInt(dfp)
      if (magic != GraphManager.Consts.BundleFileMagicNumber) {
        throw Exception(
          f"Unexpected magic number ${magic}, expecting ${GraphManager.Consts.BundleFileMagicNumber} for data file ${fileName}"
        )
      }

      val env: BundleFileEnvelope = Helpers.readLenAndCBOR(dfp)

      if (env.magic != GraphManager.Consts.BundleFileMagicNumber) {
        throw Exception(
          f"Loaded a bundle with an invalid magic number: ${env}"
        );
      }

      val indexFiles = Map((for { indexFile <- env.indexFiles } yield {

        (indexFile -> IndexFile.open(theDir, indexFile))
      }): _*)

      val dataFileHashes = Set((for {
        idx <- indexFiles.values; dataFile <- idx.envelope.dataFiles
      } yield dataFile).toSeq: _*)

      val dataFiles = Map((for {
        dataFileHash <- dataFileHashes.toSeq
      } yield (dataFileHash -> DataFile.open(theDir, dataFileHash))): _*)

      GoatRodeoBundle(
        envelope = env,
        path = theDir,
        dataFiles = dataFiles,
        indexFiles = indexFiles
      )
    }

  }
}
