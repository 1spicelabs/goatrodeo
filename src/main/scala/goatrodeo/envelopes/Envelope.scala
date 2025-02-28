package goatrodeo.envelopes

import goatrodeo.omnibor.GraphManager
import io.bullet.borer.Cbor
import io.bullet.borer.Codec
import io.bullet.borer.Decoder
import io.bullet.borer.Dom._
import io.bullet.borer.Encoder
import io.bullet.borer.Writer
import io.bullet.borer.derivation.key

import scala.collection.immutable.TreeMap
import scala.collection.immutable.TreeSet
import scala.util.Failure
import scala.util.Success
import scala.util.Try

trait EncodeCBOR {
  def encodeCBORElement(): Element
  def encodeCBOR(): Array[Byte] = Cbor.encode(encodeCBORElement()).toByteArray
}

trait DecodeCBOR[T] {
  def decodeCBORElement(in: Element): Try[T]
  def decodeCBOR(in: Array[Byte]): Try[T] = {
    for {
      elem <- Cbor.decode(in).to[Element].valueTry
      ret <- decodeCBORElement(elem)
    } yield ret
  }
}

/** Contains an MD5 hash
  *
  * @param hash
  *   the hash contained... should be 16 bytes
  */
final case class MD5(hash: Array[Byte]) extends EncodeCBOR {

  override def encodeCBORElement(): Element = {
    MapElem.Sized(
      StringElem("h") -> ArrayElem.Sized(
        hash.map(v => IntElem(v.toInt & 0xff))*
      )
    )
  }

  if (hash.length != 16) {
    throw new Exception(
      f"Failed to create an MD5 hash holder... requires 16 byte hash, but got ${hash.length}"
    )
  }

  /** Override equals to support comparing two arrays
    *
    * @param x
    * @return
    */
  override def equals(x: Any): Boolean = {
    x match {
      case MD5(data) => data.toVector == hash.toVector
      case _         => false
    }
  }

}

object MD5 extends DecodeCBOR[MD5] {

  override def decodeCBORElement(in: Element): Try[MD5] =
    in match {
      case m: MapElem =>
        val ret = for {
          e <- m.apply("h")
          arr <- e match {
            case ae: ArrayElem
                if ae.elems.length == 16 && ae.elems.forall(x =>
                  x match {
                    case IntElem(value) if value >= -127 && value <= 255 => true
                    case _ => false
                  }
                ) =>
              Some(ae)
            case _ => None
          }
        } yield {
          MD5(
            arr.elems
              .map(x => (x.asInstanceOf[IntElem].value & 0xff).toByte)
              .toArray
          )
        }
        ret match {
          case Some(ret) => Success(ret)
          case _ => Failure(new Exception(f"Failed to decode ${in} as MD5"))
        }
      case _ => Failure(new Exception(f"Failed to decode ${in} as MD5"))
    }

}

type Position = Long

type MultifilePosition = (Long, Long)
import io.bullet.borer.derivation.MapBasedCodecs.derived

case class DataFileEnvelope(
    version: Int,
    magic: Int,
    previous: Long,
    @key("depends_on") dependsOn: TreeSet[Long],
    @key("built_from_merge") builtFromMerge: Boolean,
    info: TreeMap[String, String]
) derives Codec {
  def encode(): Array[Byte] = Cbor.encode(this).toByteArray
}

object DataFileEnvelope {
  def build(
      version: Int = 1,
      magic: Int = GraphManager.Consts.DataFileMagicNumber,
      previous: Long,
      dependsOn: TreeSet[Long] = TreeSet(),
      builtFromMerge: Boolean,
      info: TreeMap[String, String] = TreeMap()
  ): DataFileEnvelope = DataFileEnvelope(
    version,
    magic,
    previous,
    dependsOn,
    builtFromMerge,
    info
  )

  def decode(bytes: Array[Byte]): Try[DataFileEnvelope] = {

    Cbor.decode(bytes).to[DataFileEnvelope].valueTry
  }
}

case class IndexFileEnvelope(
    version: Int,
    magic: Int,
    size: Int,
    @key("data_files") dataFiles: Vector[Long],
    encoding: String,
    info: TreeMap[String, String]
) derives Codec {
  def encode(): Array[Byte] = Cbor.encode(this).toByteArray
}

object IndexFileEnvelope {

  def build(
      version: Int = 2,
      magic: Int = GraphManager.Consts.IndexFileMagicNumber,
      size: Int,
      dataFiles: Vector[Long],
      encoding: String = "MD5/Long/Long",
      info: TreeMap[String, String] = TreeMap()
  ): IndexFileEnvelope = IndexFileEnvelope(
    version = version,
    magic = magic,
    size = size,
    dataFiles = dataFiles,
    encoding = encoding,
    info = info
  )

  def decode(bytes: Array[Byte]): Try[IndexFileEnvelope] =
    Cbor.decode(bytes).to[IndexFileEnvelope].valueTry
}

case class ClusterFileEnvelope(
    version: Int,
    magic: Int,
    @key("data_files") dataFiles: Vector[Long],
    @key("index_files") indexFiles: Vector[Long],
    info: TreeMap[String, String]
) {
  def encode(): Array[Byte] = Cbor.encode(this).toByteArray

}

object ClusterFileEnvelope {
  def build(
      version: Int = 2,
      magic: Int = GraphManager.Consts.ClusterFileMagicNumber,
      dataFiles: Vector[Long],
      indexFiles: Vector[Long],
      info: TreeMap[String, String] = TreeMap()
  ): ClusterFileEnvelope = ClusterFileEnvelope(
    version = version,
    magic = magic,
    dataFiles = dataFiles,
    indexFiles = indexFiles,
    info = info
  )

  given forOption[T: Encoder]: Encoder.DefaultValueAware[Option[T]] =
    new Encoder.DefaultValueAware[Option[T]] {

      def write(w: Writer, value: Option[T]): Writer =
        value match {
          case Some(x) => w.write(x)
          case None    => w.writeNull()
        }

      def withDefaultValue(defaultValue: Option[T]): Encoder[Option[T]] =
        if (defaultValue eq None)
          new Encoder.PossiblyWithoutOutput[Option[T]] {
            def producesOutputFor(value: Option[T]): Boolean = value ne None
            def write(w: Writer, value: Option[T]): Writer =
              value match {
                case Some(x) => w.write(x)
                case None    => w
              }
          }
        else this
    }

  given Codec[ClusterFileEnvelope] = {
    import io.bullet.borer.derivation.MapBasedCodecs.*
    deriveCodec[ClusterFileEnvelope]
  }
}
