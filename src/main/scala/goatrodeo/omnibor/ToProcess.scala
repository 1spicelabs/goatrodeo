package goatrodeo.omnibor

import goatrodeo.util.ArtifactWrapper
import com.github.packageurl.PackageURL
import java.io.File
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean
import goatrodeo.util.Helpers
import goatrodeo.util.FileWrapper
import com.typesafe.scalalogging.Logger
import goatrodeo.util.GitOID
import goatrodeo.util.GitOIDUtils
import goatrodeo.omnibor.ToProcess.ByUUID
import goatrodeo.omnibor.ToProcess.ByName
import goatrodeo.omnibor.Item.noopLocationReference
import scala.collection.immutable.TreeSet
import scala.collection.immutable.TreeMap
import goatrodeo.util.FileWalker
import java.io.BufferedReader
import java.io.InputStreamReader
import goatrodeo.util.PURLHelpers
import goatrodeo.util.PURLHelpers.Ecosystems
import goatrodeo.omnibor.strategies.*

/** When processing Artifacts, knowing the Artifact type for a sequence of
  * artifacts can be helpful. For example (Java POM File, Java Sources,
  * JavaDocs, JAR)
  *
  * Each Strategy will have a Processing State and that's associated with the
  * marker
  */
trait ProcessingMarker

/** Sometimes you don't need a marker, so just use this one
  */
final class SingleMarker extends ProcessingMarker

/** For each of the processing strategies, keeping state along with the
  * processing (for example, for JVM stuff, processing the POM file is a step in
  * the set of files to process. Keeping the pom file and information from the
  * pom file allows for the strategy to "do the right thing")
  */
trait ProcessingState[PM <: ProcessingMarker, ME <: ProcessingState[PM, ME]] {

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
      marker: PM
  ): ME

  /** Gets the package URLs for the given artifact
    *
    * @param artifact
    *   the artifact
    * @param item
    *   the `Item` that's currently under construction
    * @param marker
    *   the marker (e.g., a pom vs javadoc vs. JAR marker)
    * @return
    *   the computed Package URLs as any update to the State object
    */
  def getPurls(
      artifact: ArtifactWrapper,
      item: Item,
      marker: PM
  ): (Vector[PackageURL], ME)

  /** Get the `extra` information for the artifact
    *
    * @param artifact
    *   the artifact to extract the information
    * @param item
    *   the `Item` being constructed
    * @param marker
    *   the marker
    * @return
    *   the extra information and the new state
    */
  def getMetadata(
      artifact: ArtifactWrapper,
      item: Item,
      marker: PM
  ): (TreeMap[String, TreeSet[StringOrPair]], ME)

  /** If there's any final augmentation to do on an item
    *
    * @param artifact
    *   the artifact being processed
    * @param item
    *   the Item as currently constructed
    * @param marker
    *   the marker
    * @return
    *   the updated item and the updated state
    */
  def finalAugmentation(
      artifact: ArtifactWrapper,
      item: Item,
      marker: PM
  ): (Item, ME)

  /** Called after processing childred at a particular state. This will allow
    * capturing the Java sources that were processed after processing the
    * children of a `-sources.jar` file
    *
    * @param kids
    *   the gitoids that are children of the currently processed item
    * @param store
    *   the `Storage` instance
    * @param marker
    *   the marker
    * @return
    *   an updated state
    */
  def postChildProcessing(
      kids: Option[Vector[GitOID]],
      store: Storage,
      marker: PM
  ): ME

  def generateParentScope(
      artifact: ArtifactWrapper,
      item: Item,
      store: Storage,
      marker: PM
  ): ParentScope = ParentScope.empty
}

trait ParentScope {
  def beginProcessing(artifact: ArtifactWrapper, item: Item): Item = item
  def enhanceWithPurls(artifact: ArtifactWrapper,item: Item, purls: Vector[PackageURL]): Item = item
  def enhanceWithMetadata(artifact: ArtifactWrapper,
      item: Item,
      metadata: TreeMap[String, TreeSet[StringOrPair]],
      paths: Vector[String]
  ): Item = item
  def finalAugmentation(artifact: ArtifactWrapper,item: Item): Item = item
  def postFixReferences(artifact: ArtifactWrapper,item: Item): Unit = ()
}

object ParentScope {
  def empty: ParentScope = new ParentScope{}
}

/** A file or set of files to process
  */
trait ToProcess {
  type MarkerType <: ProcessingMarker
  type StateType <: ProcessingState[MarkerType, StateType]

  /** The name of the main artifact
    */
  def main: String

  /** The mime type of the main artifact
    */
  def mimeType: String

  /**
   * The number of items (e.g., jar, sources, pom) in this process bundle
   */
  def itemCnt: Int

  /** Return the list of artifacts to process along with a `MarkerType` and an
    * initial state
    */
  def getElementsToProcess(): (Seq[(ArtifactWrapper, MarkerType)], StateType)

  /** Recursively process
    *
    * @param parentId
    * @param store
    * @param parentScope
    * @param purlOut
    * @param blockList
    * @param keepRunning
    * @param atEnd
    * @return
    */
  def process(
      parentId: Option[GitOID],
      store: Storage,
      parentScope: ParentScope = ParentScope.empty,
      purlOut: PackageURL => Unit = _ => (),
      blockList: Set[GitOID] = Set(),
      keepRunning: () => Boolean = () => true,
      atEnd: (Option[GitOID], Item) => Unit = (_, _) => ()
  ): Seq[GitOID] = {
    if (keepRunning()) {

      val (elements, initialState) = getElementsToProcess()
      val (finalState, ret) =
        elements.foldLeft(initialState -> Vector[GitOID]()) {
          case ((orgState, alreadyDone), (artifact, marker)) =>
            val item = Item.itemFrom(artifact, parentId)

            // in blocklist do nothing
            if (blockList.contains(item.identifier)) {
              orgState -> alreadyDone
            } else {
              val state = orgState.beginProcessing(artifact, item, marker)
              val itemScope1 = parentScope.beginProcessing(artifact, item)
              // get purls
              val (purls, state2) = state.getPurls(artifact, itemScope1, marker)

              // enhance with package URLs
              val item2 = itemScope1.enhanceItemWithPurls(purls)
             
              val itemScope2 = parentScope.enhanceWithPurls(artifact, item2, purls)

              // compute metadata
              val (metadata, state3) =
                state2.getMetadata(artifact, itemScope2, marker)

                // update metadata
              val item3 =
                itemScope2.enhanceWithMetadata(metadata, Vector(artifact.path()))

              val itemScope3 = parentScope.enhanceWithMetadata(
                artifact,
                item3,
                metadata,
                Vector(artifact.path())
              )

              // do final augmentation (e.g., mapping source to classes)
              val (item4, state4) =
                state3.finalAugmentation(artifact, itemScope3, marker)

              val itemScope4 = parentScope.finalAugmentation(artifact, item4)

              // if we've seen the gitoid before we write it
              val hasBeenSeen = store.contains(itemScope4.identifier)

              // write
              val answerItem = store.write(
                itemScope4.identifier,
                {
                  case None            => itemScope4
                  case Some(otherItem) => otherItem.merge(itemScope4)
                }
              )

              // update purls
              purls.foreach(purlOut)

              // fix the references
              answerItem.fixReferences(store)

              parentScope.postFixReferences(artifact, answerItem)

              atEnd(parentId, answerItem)

              val childGitoids: Option[Vector[GitOID]] =
                // if the gitoid has already been seen, do not recurse into the potential child
                if (hasBeenSeen) None
                else {
                  FileWalker.withinArchiveStream(artifact = artifact) {
                    foundItems =>
                      val processSet =
                        ToProcess.strategiesForArtifacts(foundItems, x => ())
                      val parentScope = state4.generateParentScope(
                        artifact,
                        answerItem,
                        store,
                        marker
                      )
                      processSet.flatMap(tp =>
                        tp.process(
                          Some(answerItem.identifier),
                          store,
                          parentScope,
                          purlOut,
                          blockList,
                          keepRunning,
                          atEnd
                        )
                      )
                  }
                }

              val state5 =
                state4.postChildProcessing(childGitoids, store, marker)

              state5 -> (alreadyDone :+ answerItem.identifier)
            }
        }

      ret
    } else Vector.empty
  }
}

object ToProcess {
  val logger = Logger(getClass())

  type ByUUID = Map[String, ArtifactWrapper]
  type ByName = Map[String, Vector[ArtifactWrapper]]

  val computeToProcess: Vector[
    (ByUUID, ByName) => (Vector[ToProcess], ByUUID, ByName, String)
  ] =
    Vector(
      MavenToProcess.computeMavenFiles,
      Debian.computeDebianFiles,
      GenericFile.computeGenericFiles
    )

  /** Given a directory, find all the files and create the strategies for
    * processing
    *
    * @param directory
    *   the root directory
    * @param onFound
    *   a function to call on finding a strategy for each item
    * @return
    *   the set of items to process
    */
  def strategyForDirectory(
      directory: File,
      onFound: ToProcess => Unit = _ => ()
  ): Vector[ToProcess] = {
    val wrappers = Helpers
      .findFiles(directory, _ => true)
      .map(f => FileWrapper(f, f.getPath()))

    strategiesForArtifacts(wrappers, onFound = onFound)
  }

  def strategiesForArtifacts(
      artifacts: Seq[ArtifactWrapper],
      onFound: ToProcess => Unit
  ): Vector[ToProcess] = {

    logger.trace("Creating strategies for artifacts")
    // create the list of the files
    val byUUID: ByUUID = Map(
      artifacts.map(f => f.uuid -> f)*
    )

    // and by name for lookup
    val byName: ByName =
      artifacts.foldLeft(Map()) { case (map, wrapper) =>
        val v = map.get(wrapper.filenameWithNoPath) match {
          case None    => Vector()
          case Some(v) => v
        }
        map + (wrapper.filenameWithNoPath -> (v :+ wrapper))
      }

    logger.trace("Finished setting up files for per-ecosystem specialization")

    val (processSet, finalByUUID, finalByName) =
      computeToProcess.zipWithIndex.foldLeft(
        (Vector[ToProcess](), byUUID, byName)
      ) { case ((workingSet, workingByUUID, workingByName), (theFn, cnt)) =>
        logger.trace(f"Processing step ${cnt + 1}")
        val (addlToProcess, revisedByUUID, revisedByName, name) =
          theFn(workingByUUID, workingByName)
        logger.trace(
          f"Finished processing step ${cnt + 1} for ${name} found ${addlToProcess.length}"
        )

        // put the new items to process in the queue
        addlToProcess.foreach(onFound)

        (
          workingSet ++ addlToProcess,
          revisedByUUID,
          revisedByName.filter((_, items) => !items.isEmpty)
        )
      }

    processSet

  }

  def buildQueueOnSeparateThread(
      root: File,
      count: AtomicInteger
  ): (ConcurrentLinkedQueue[ToProcess], AtomicBoolean) = {
    val stillWorking = AtomicBoolean(true)
    val queue = ConcurrentLinkedQueue[ToProcess]()
    val buildIt: Runnable = () => {
      try {
        // get all the files
        val allFiles: Vector[ArtifactWrapper] = Helpers
          .findFiles(root, _ => true)
          .map(f => FileWrapper(f, f.getPath()))

        logger.info(f"Found all files in ${root}, count ${allFiles.length}")

        strategiesForArtifacts(
          allFiles,
          toProcess => {
            queue.add(toProcess)
            count.addAndGet(toProcess.itemCnt)
          }
        )

        logger.info("Finished setting files up")
      } catch {
        case e: Exception =>
          logger.error(f"Failed to build graph ${e.getMessage()}")
      } finally {

        stillWorking.set(false)
      }
    }

    val t = Thread(buildIt, "File Finder")
    t.start()

    (queue, stillWorking)
  }

  /** Build the graph for a collection to `ToProcess` items
    *
    * @param toProcess
    *   the items to process
    * @param store
    *   the store
    * @param purlOut
    *   the package URL destination
    * @param block
    *   the list of gitoids to block
    * @return
    *   the store and package url destination
    */
  def buildGraphForToProcess(
      toProcess: Vector[ToProcess],
      store: Storage = MemStorage(None),
      purlOut: PackageURL => Unit = _ => (),
      block: Set[GitOID] = Set()
  ): Storage = {
    for { individual <- toProcess } {
      individual.process(None, store, purlOut = purlOut, blockList = block)
    }

    store
  }

  /** Build the graph for a single ArtifactWrapper by creating a strategy for
    * the wrapper and processing the items in the strategy
    *
    * @param artifact
    *   the artifact to process
    * @param store
    *   the store
    * @param purlOut
    *   the package URL destination
    * @param block
    *   the list of gitoids to block
    * @return
    *   the store and package url destination
    */
  def buildGraphFromArtifactWrapper(
      artifact: ArtifactWrapper,
      store: Storage = MemStorage(None),
      purlOut: PackageURL => Unit = _ => (),
      block: Set[GitOID] = Set()
  ): Storage = {

    // generate the strategy
    val toProcess = strategiesForArtifacts(Vector(artifact), _ => ())

    buildGraphForToProcess(toProcess, store, purlOut, block)

  }
}
