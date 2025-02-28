package goatrodeo.omnibor.strategies

import com.github.packageurl.PackageURL
import com.typesafe.scalalogging.Logger
import goatrodeo.omnibor.EdgeType
import goatrodeo.omnibor.Item
import goatrodeo.omnibor.ParentScope
import goatrodeo.omnibor.ProcessingMarker
import goatrodeo.omnibor.ProcessingState
import goatrodeo.omnibor.Storage
import goatrodeo.omnibor.StringOrPair
import goatrodeo.omnibor.ToProcess
import goatrodeo.omnibor.ToProcess.ByName
import goatrodeo.omnibor.ToProcess.ByUUID
import goatrodeo.util.ArtifactWrapper
import goatrodeo.util.FileWalker
import goatrodeo.util.GitOID
import goatrodeo.util.Helpers
import goatrodeo.util.PURLHelpers
import goatrodeo.util.PURLHelpers.Ecosystems

import java.util.Properties
import scala.collection.immutable.TreeMap
import scala.collection.immutable.TreeSet
import scala.util.Try
import scala.xml.NodeSeq
import scala.xml.XML

enum MavenMarkers extends ProcessingMarker {
  case POM
  case JAR
  case Sources
  case JavaDocs
}

case class MavenState(
    pomFile: String = "",
    pomXml: NodeSeq = NodeSeq.Empty,
    sources: Map[String, Item] = Map(),
    sourceGitoids: Map[String, GitOID] = Map()
) extends ProcessingState[MavenMarkers, MavenState] {
  private lazy val logger = Logger(getClass())

  /** Call the state object at the beginning of processing an ArtfactWrapper
    * into an Item. This is done just after the generation of the gitoids.
    *
    * This allows state to capture, for example, the contents of a pom file
    *
    * @param artifact
    *   the artifact
    * @param item
    *   the currently build item
    * @param marker
    *   the marker
    * @return
    *   the updated state
    */
  def beginProcessing(
      artifact: ArtifactWrapper,
      item: Item,
      marker: MavenMarkers
  ): MavenState = marker match {
    case MavenMarkers.POM =>
      val pomString = Helpers.slurpInputToString(artifact.asStream())
      val xml =
        Try { XML.loadString(pomString) }.toOption.getOrElse(NodeSeq.Empty)
      this.copy(pomFile = pomString, pomXml = xml)

    case _ => this
  }
  override def getPurls(
      artifact: ArtifactWrapper,
      item: Item,
      marker: MavenMarkers
  ): (Vector[PackageURL], MavenState) = {
    val grp = findTag(pomXml, "groupId")
    val art = findTag(pomXml, "artifactId")
    val ver = tryToFixVersion(
      findTag(pomXml, "version"),
      artifact.filenameWithNoPath
    )

    (grp, art, ver) match {
      case (Some(g), Some(a), Some(v)) =>
        val ret = Vector(
          PURLHelpers.buildPackageURL(
            Ecosystems.Maven,
            Some(g),
            a,
            v,
            marker match {
              case MavenMarkers.JAR      => None
              case MavenMarkers.Sources  => Some("sources")
              case MavenMarkers.POM      => Some("pom")
              case MavenMarkers.JavaDocs => Some("javadoc")
            }
          )
        )
        ret -> this
      case _ => (Vector(), this)

    }
  }

  private def findTag(in: NodeSeq, name: String): Option[String] = {
    val topper = in \ name
    if (topper.length == 1) {
      topper.text match {
        case s if s.length() > 0 => Some(s)
        case _                   => None
      }

    } else {
      val t2 = in \ "parent" \ name
      if (t2.length == 1 && t2.text.length() > 0) { Some(t2.text) }
      else None
    }
  }

  private def tryToFixVersion(
      in: Option[String],
      fileName: String
  ): Option[String] = {
    in match {
      case Some(s) if s.startsWith("${") =>
        val fn2 =
          fileName.substring(0, fileName.length() - 4) // remove ".pom"
        val li = fn2.lastIndexOf("-")
        if (li >= 0) {
          Some(fn2.substring(li + 1))
        } else in
      case _ => in
    }
  }

  override def getMetadata(
      artifact: ArtifactWrapper,
      item: Item,
      marker: MavenMarkers
  ): (TreeMap[String, TreeSet[StringOrPair]], MavenState) = {
    import scala.jdk.CollectionConverters.*

    val baseTree = if (pomFile.length() > 4) {
      TreeMap(
        "pom" -> TreeSet(StringOrPair("text/xml", pomFile))
      )
    } else TreeMap[String, TreeSet[StringOrPair]]()

    val manifest: TreeMap[String, TreeSet[StringOrPair]] = marker match {
      case MavenMarkers.POM => TreeMap()
      case _ =>
        FileWalker
          .withinArchiveStream(artifact) { files =>
            files
              .filter(_.path().toUpperCase() == "META-INF/MANIFEST.MF")
              .headOption match {
              case Some(manifest) =>
                val manifestString =
                  Helpers.slurpInputToString(manifest.asStream())
                val props = java.util.Properties()
                // if this causes an exception, log it.
                try {
                  props.load(manifest.asStream())
                } catch {
                  case e: Exception =>
                    logger.error(
                      f"Failed to parse `META-INF/MANIFEST.MF` for ${artifact
                          .path()}, error ${e.getMessage()}"
                    )
                }

                val ret = TreeMap(
                  props
                    .keys()
                    .asIterator()
                    .asScala
                    .map(k =>
                      k.asInstanceOf[String]
                        .toLowerCase /*.intern()*/ -> TreeSet(
                        StringOrPair(props.get(k).asInstanceOf[String])
                      )
                    )
                    .toSeq*
                )

                ret + ("manifest" -> TreeSet(
                  StringOrPair("text/maven-manifest", manifestString)
                ))

              case None => TreeMap[String, TreeSet[StringOrPair]]()
            }
          }
          .getOrElse(TreeMap[String, TreeSet[StringOrPair]]())
    }

    Helpers.mergeTreeMaps(baseTree, manifest) -> this
  }

  override def finalAugmentation(
      artifact: ArtifactWrapper,
      item: Item,
      marker: MavenMarkers
  ): (Item, MavenState) = item -> this

  override def postChildProcessing(
      kids: Option[Vector[GitOID]],
      store: Storage,
      marker: MavenMarkers
  ): MavenState = (marker, kids) match {
    case (MavenMarkers.Sources, Some(kids)) =>
      val items = for {
        gitoid <- kids
        item <- store.read(gitoid).toVector
        metadata <- item.body.toVector
        filename <- metadata.fileNames.toVector
      } yield filename -> item
      val ret = this.copy(
        sources = Map(items*),
        sourceGitoids = Map(items.map { case (k, v) => k -> v.identifier }*)
      )
      ret
    case _ => this
  }

  override def generateParentScope(
      artifact: ArtifactWrapper,
      item: Item,
      store: Storage,
      marker: MavenMarkers,
      parentScope: Option[ParentScope]
  ): ParentScope = marker match {
    case MavenMarkers.JAR =>
      // the code that associates source with class files
      new ParentScope {

        def scopeFor(): String = item.identifier
        def parentOfParentScope(): Option[ParentScope] = parentScope

        def parentScopeInformation(): String =
          f"Maven/JAR Scope for ${item.identifier}${parentScope match {
              case None     => ""
              case Some(ps) => f" Parent: ${ps.parentScopeInformation()}"
            }}"

        override def finalAugmentation(
            store: Storage,
            artifact: ArtifactWrapper,
            item: Item
        ): Item = {
          val sources = Helpers.computeAssociatedSource(
            artifact,
            associatedFiles = sourceGitoids
          )
          sources.foldLeft(item) { case (item, source) =>
            item.withConnection(EdgeType.builtFrom, source)
          }
        }
      }
    case _ => ParentScope.forAndWith(item.identifier, parentScope)
  }

}

final case class MavenToProcess(
    jar: ArtifactWrapper,
    pom: Option[ArtifactWrapper],
    source: Option[ArtifactWrapper],
    javaDoc: Option[ArtifactWrapper]
) extends ToProcess {
  type MarkerType = MavenMarkers
  type StateType = MavenState

  override def itemCnt: Int = {
    def bToI(b: Boolean): Int = if (b) 1 else 0
    1 + bToI(pom.isDefined) + bToI(source.isDefined) + bToI(javaDoc.isDefined)
  }
  override def main: String = jar.path()

  /** The mime type of the main artifact
    */
  def mimeType: String = jar.mimeType

  override def getElementsToProcess()
      : (Seq[(ArtifactWrapper, MarkerType)], StateType) = Vector(
    pom.toVector.map(pom => pom -> MavenMarkers.POM),
    source.toVector.map(src => src -> MavenMarkers.Sources),
    javaDoc.toVector.map(jd => jd -> MavenMarkers.JavaDocs),
    Vector(jar -> MavenMarkers.JAR)
  ).flatten -> MavenState()

}

object MavenToProcess {
  val logger: Logger = Logger(getClass())
  def computeMavenFiles(
      byUUID: ToProcess.ByUUID,
      byName: ToProcess.ByName
  ): (Vector[ToProcess], ByUUID, ByName, String) = {
    val jars = byName.toVector.filter { case (name, artifacts) =>
      name.endsWith(".jar") && !(name.endsWith("-sources.jar") || name.endsWith(
        "-javadoc.jar"
      )) &&
      !artifacts.filter(_.mimeType == "application/java-archive").isEmpty
    }

    val (toProcess, revisedByUUID, revisedByName) =
      jars.foldLeft((Vector[ToProcess](), byUUID, byName)) {
        case ((toProcess, byId, byName), (name, artifacts)) =>
          val noJarName = name.substring(0, name.length() - 4)
          val pomName = noJarName + ".pom"
          val javaDocName = noJarName + "-javadoc.jar"
          val sourcesName = noJarName + "-sources.jar"

          val poms = byName.get(pomName).toVector.flatten
          val javaDocs = byName.get(javaDocName).toVector.flatten
          val sources = byName.get(sourcesName).toVector.flatten
          val revisedById =
            Vector(artifacts, poms, sources, javaDocs).flatten.foldLeft(byId) {
              case (byId, artifact) => byId - artifact.uuid
            }
          val revisedToProcess = toProcess :+ MavenToProcess(
            artifacts.head,
            poms.headOption,
            sources.headOption,
            javaDocs.headOption
          )
          (
            revisedToProcess,
            revisedById,
            byName - name - pomName - javaDocName - sourcesName
          )
      }

    (toProcess, revisedByUUID, revisedByName, "Maven")
  }
}
