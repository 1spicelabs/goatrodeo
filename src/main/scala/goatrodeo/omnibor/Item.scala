package goatrodeo.omnibor

import com.github.packageurl.PackageURL
import com.typesafe.scalalogging.Logger
import goatrodeo.util.ArtifactWrapper
import goatrodeo.util.GitOID
import goatrodeo.util.GitOIDUtils
import goatrodeo.util.Helpers
import io.bullet.borer.Cbor
import io.bullet.borer.Decoder
import io.bullet.borer.Encoder
import io.bullet.borer.Writer
import io.bullet.borer.derivation.key

import scala.collection.immutable.TreeMap
import scala.collection.immutable.TreeSet
import scala.util.Try

case class Item(
    identifier: String,
    // reference: LocationReference,
    connections: TreeSet[Edge],
    @key("body_mime_type") bodyMimeType: Option[String],
    body: Option[ItemMetaData]
) {
  def encodeCBOR(): Array[Byte] = cachedCBOR

  // def fixReferencePosition(hash: Long, offset: Long): Item = {
  //   val hasCur = reference != Item.noopLocationReference
  //   this.copy(
  //     reference = (hash, offset)
  //   )
  // }

  /** add a connection
    *
    * @param edgeType
    *   the type of connection
    * @param id
    *   the id of the connection
    * @return
    *   the revised Item
    */
  def withConnection(edgeType: String, id: String): Item =
    this.copy(connections = this.connections + (edgeType -> id))

  private lazy val md5 = Helpers.computeMD5(identifier)

  lazy val cachedCBOR: Array[Byte] = Cbor.encode(this).toByteArray

  def identifierMD5(): Array[Byte] = md5

  def cmpMd5(that: Item): Boolean = {
    val myHash = Helpers.md5hashHex(identifier)
    val thatHash = Helpers.md5hashHex(that.identifier)
    myHash < thatHash
  }

  /** Builds a list of items that are referenced from this item. The references
    * are of types `AliasFrom`, `BuiltFrom`, and `ContainedBy`
    *
    * The resulting `Item`s should be updated in the store
    */
  def buildListOfReferencesForAliasFromBuiltFromContainedBy()
      : Vector[(String, String)] = {
    for {
      edge <- this.connections.toVector
      toUpdate <- edge match {
        case Edge(EdgeType.aliasFrom, connection) => {
          Vector(
            (EdgeType.aliasTo -> connection)
          )

        }

        case Edge(EdgeType.builtFrom, connection) => {

          Vector(EdgeType.buildsTo -> connection)

        }
        case Edge(EdgeType.containedBy, connection) => {

          Vector(
            EdgeType.contains -> connection
          )
        }
        case _ => Vector.empty
      }

    } yield {
      toUpdate
    }
  }

  /** Create or update (merge) this `Item` in the store.
    *
    * The resulting item will be returned. The resulting `Item` may be `this` or
    * `this` merged with the item in the store
    *
    * @param store
    *   the `Storage` instance
    *
    * @return
    *   the updated item
    */
  def createOrUpdateInStore(store: Storage, context: Item => String): Item = {
    store.write(
      identifier,
      {
        case None        => this
        case Some(other) => this.merge(other)
      },
      context
    )
  }

  /** Merge this `Item` with another `Item`
    *
    * @param other
    *   the item to merge with
    *
    * @return
    *   the merged items
    */
  def merge(other: Item): Item = {

    val (body, mime) =
      (this.body, other.body, this.bodyMimeType == other.bodyMimeType) match {
        case (Some(a), Some(b), true) => Some(a.merge(b)) -> this.bodyMimeType
        case (Some(a), _, _)          => Some(a) -> this.bodyMimeType
        case (_, Some(b), _)          => Some(b) -> other.bodyMimeType
        case _                        => None -> None
      }

    Item(
      identifier = this.identifier,
      // reference = this.reference,
      connections = this.connections ++ other.connections,
      bodyMimeType = mime,
      body = body
    )

  }

  /** Given an `Item`, enhance it with PackageURLs
    *
    * @param purls
    *   the PackageURLs
    * @param fileNames
    *   the filenames associated with this Item
    *
    * @return
    *   the enhanced `Item`
    */
  def enhanceItemWithPurls(purls: Seq[PackageURL]): Item = {
    if (purls.isEmpty) this
    else {

      val textPurls = purls.map(p => p.canonicalize() /*.intern() */ )
      val ret = this.copy(
        connections = this.connections ++ TreeSet(
          textPurls.map(purl => EdgeType.aliasFrom -> purl)*
        ),
        body = this.body match {
          case None =>
            Some(
              ItemMetaData(
                fileNames = TreeSet(textPurls*),
                mimeType = TreeSet(),
                fileSize = 0,
                extra = TreeMap()
              )
            )
          case Some(body) =>
            Some(body.copy(fileNames = body.fileNames ++ textPurls))
        }
      )
      ret
    }
  }

  /** Enhance an `Item` with metadata including filenames and extra
    *
    * @param extra
    *   the extra metadata to add
    * @param filenames
    *   the extra filenames to add
    *
    * @return
    *   the enhanced `Item`
    */
  def enhanceWithMetadata(
      extra: TreeMap[String, TreeSet[StringOrPair]],
      filenames: Seq[String] = Vector()
  ): Item = {
    this.copy(
      bodyMimeType = this.bodyMimeType match {
        case Some(v) => Some(v)
        case None    => Some(ItemMetaData.mimeType)
      },
      body = Some({
        val base = this.body match {
          case Some(b) => b
          case None =>
            ItemMetaData(
              fileNames = TreeSet(),
              mimeType = TreeSet(),
              fileSize = 0,
              extra = TreeMap()
            )
        }
        base.copy(
          fileNames = base.fileNames ++ filenames,
          extra = base.extra ++ extra
        )
      })
    )
  }
}

object Item {
  protected val logger: Logger = Logger(getClass())

  /** Given an ArtifactWrapper, create an `Item` based on the hashes/gitoids for
    * the artifact
    *
    * @param artifact
    *   the artifact to compute the Item for
    * @param container
    *   if this item is contained by another item, include that
    *
    * @return
    *   the created item
    */
  def itemFrom(artifact: ArtifactWrapper, container: Option[GitOID]): Item = {
    val (id, hashes) = GitOIDUtils.computeAllHashes(artifact)
    Item(
      id,
      // Item.noopLocationReference,
      TreeSet(
        hashes.map(hash => EdgeType.aliasFrom -> hash)*
      ) ++ container.toSeq.map(c => EdgeType.containedBy -> c),
      Some(ItemMetaData.mimeType),
      Some(
        ItemMetaData(
          fileNames = TreeSet(),
          mimeType = TreeSet(artifact.mimeType),
          fileSize = artifact.size(),
          extra = TreeMap()
        )
      )
    )
  }

  /** Given `Item`s, create a mapping between the filenames and the GitOIDs for
    * the item. Used as part of the Maven/JVM source mapping features.
    *
    * @param items
    *   the items to map
    * @param nameFilter
    *   allows filtering of the name (e.g., remove pURL, etc.)
    * @param mimeFilter
    *   allows filtering of mime type to ensure it's a mapping to the mime types
    *   that are interesting
    *
    * @return
    *   a map from filenames to gitoids
    */
  def itemsToFilenameGitOIDMap(
      items: Seq[Item],
      nameFilter: String => Boolean = s => true,
      mimeFilter: Set[String] => Boolean = s => true
  ): Map[String, GitOID] = {
    val mapping = for {
      item <- items
      metadata <- item.body.toSeq if mimeFilter(metadata.mimeType)
      filename <- metadata.fileNames.toSeq if nameFilter(filename)
    } yield filename -> item.identifier

    Map(mapping*)
  }

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

  val noopLocationReference: LocationReference = (0L, 0L)
  given Encoder[Item] = {
    import io.bullet.borer.derivation.MapBasedCodecs.*
    deriveEncoder[Item]
  }

  given Decoder[Item] = {
    import io.bullet.borer.derivation.MapBasedCodecs.*
    deriveDecoder[Item]
  }

  def decode(bytes: Array[Byte]): Try[Item] = {
    Cbor.decode(bytes).to[Item].valueTry
  }
}
