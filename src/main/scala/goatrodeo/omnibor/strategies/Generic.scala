package goatrodeo.omnibor.strategies

import com.github.packageurl.PackageURL
import goatrodeo.omnibor.Item
import goatrodeo.omnibor.ProcessingState
import goatrodeo.omnibor.SingleMarker
import goatrodeo.omnibor.Storage
import goatrodeo.omnibor.StringOrPair
import goatrodeo.omnibor.ToProcess
import goatrodeo.omnibor.ToProcess.ByName
import goatrodeo.omnibor.ToProcess.ByUUID
import goatrodeo.util.ArtifactWrapper
import goatrodeo.util.GitOID

import scala.collection.immutable.TreeMap
import scala.collection.immutable.TreeSet

class GenericFileState extends ProcessingState[SingleMarker, GenericFileState] {

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
      marker: SingleMarker
  ): GenericFileState = this

  override def getPurls(
      artifact: ArtifactWrapper,
      item: Item,
      marker: SingleMarker
  ): (Vector[PackageURL], GenericFileState) = Vector.empty -> this

  override def getMetadata(
      artifact: ArtifactWrapper,
      item: Item,
      marker: SingleMarker
  ): (TreeMap[String, TreeSet[StringOrPair]], GenericFileState) =
    TreeMap[String, TreeSet[StringOrPair]]() -> this

  override def finalAugmentation(
      artifact: ArtifactWrapper,
      item: Item,
      marker: SingleMarker
  ): (Item, GenericFileState) = item -> this

  override def postChildProcessing(
      kids: Option[Vector[GitOID]],
      store: Storage,
      marker: SingleMarker
  ): GenericFileState = this

}

final case class GenericFile(file: ArtifactWrapper) extends ToProcess {
  type MarkerType = SingleMarker
  type StateType = GenericFileState
  override def main: String = file.path()

  override def itemCnt: Int = 1

  /** The mime type of the main artifact
    */
  def mimeType: String = file.mimeType

  override def getElementsToProcess()
      : (Seq[(ArtifactWrapper, MarkerType)], StateType) =
    Vector(file -> SingleMarker()) -> GenericFileState()

}

object GenericFile {
  def computeGenericFiles(
      byUUID: ToProcess.ByUUID,
      byName: ToProcess.ByName
  ): (Vector[ToProcess], ByUUID, ByName, String) = {
    val ret = for {
      vals <- byName.values
      wrapper <- vals
    } yield GenericFile(wrapper)

    (ret.toVector, Map(), Map(), "Generic")
  }
}
