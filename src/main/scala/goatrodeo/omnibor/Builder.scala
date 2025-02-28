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

package goatrodeo.omnibor

import com.typesafe.scalalogging.Logger
import goatrodeo.util.GitOIDUtils
import goatrodeo.util.Helpers

import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import java.io.IOException
import java.nio.file.Files
import java.time.Duration
import java.time.Instant
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import scala.collection.parallel.CollectionConverters.VectorIsParallelizable
import scala.util.Try

/** Build the GitOIDs the container and all the sub-elements found in the
  * container
  */
object Builder {
  val logger: Logger = Logger(getClass())

  /** Build the OmniBOR GitOID Corpus from all the files contained in the
    * directory and its subdirectories. Put the results in Storage.
    *
    * @param source
    *   where to search for files
    * @param storage
    *   the storage destination of the corpus
    * @param threadCnt
    *   the number of threads to use when computings
    * @param blockList
    *   a file containing gitoids to not process (e.g., Apache license files)
    * @param maxRecords
    *   the maximum number of records to process at once
    */
  def buildDB(
      source: File,
      dest: File,
      // storage: Storage,
      threadCnt: Int,
      blockList: Option[File],
      maxRecords: Int
  ): Unit = {
    val totalStart = Instant.now()

    val runningCnt = AtomicInteger(0)
    val (queue, stillWorking) =
      ToProcess.buildQueueOnSeparateThread(source, runningCnt)

    // The count of all the files found
    val cnt = new AtomicInteger(0)

    // Get the gitoids to block
    val blockGitoids: Set[String] = blockList match {
      case None => Set()
      case Some(file) =>
        Try {
          import scala.jdk.CollectionConverters.CollectionHasAsScala

          val lines = Files.readAllLines(file.toPath()).asScala.toSet
          lines
        }.toOption match {
          case None    => Set()
          case Some(s) => s
        }

    }

    logger.info(
      "Blocking build db until there's at least 1 item in the work queue"
    )
    // Don't kick off the consumer threads until at least 1 item
    // is in the work queue
    while (queue.isEmpty()) {
      Thread.sleep(250)
    }

    logger.info("Kicking off work queue consumer threads")
    val loopStart = Instant.now()
    var updatedDest = dest

    def destWithCount(dest: File, cnt: Int): File = {
      val destFileName = dest.getName()
      File(dest.getParentFile(), f"${destFileName}_${cnt}")
    }

    if (runningCnt.get() > maxRecords) {
      updatedDest = destWithCount(dest, 0)
    }

    var loopCnt = 0
    val dead_? = AtomicBoolean(false)
    val writeThreadCnt = AtomicInteger(0)
    while (!dead_?.get() && (stillWorking.get() || !queue.isEmpty())) {
      val thread = processMaxRecords(
        updatedDest,
        threadCnt = threadCnt,
        maxRecords = maxRecords,
        queue = queue,
        stillWorking = stillWorking,
        blockGitoids = blockGitoids,
        cnt = cnt,
        runningCnt = runningCnt,
        totalStart = totalStart,
        dead_? = dead_?,
        loopStart = loopStart,
        writeThreadCnt = writeThreadCnt
      )

      loopCnt += 1
      logger.info(
        f"Finished multi-thread consumer loop ${loopCnt} at ${Duration
            .between(totalStart, Instant.now())}"
      )
      updatedDest = destWithCount(dest, loopCnt)
    }

    logger.info("Waiting for write threads")

    // loop while we wait for the end of processing
    while (writeThreadCnt.get() > 0) {
      Thread.sleep(250)
    }

    // create the non-number-suffixed directory
    dest.mkdirs()

    logger.info("Done with run")
  }

  private def processMaxRecords(
      destDir: File,
      threadCnt: Int,
      maxRecords: Int,
      queue: ConcurrentLinkedQueue[ToProcess],
      stillWorking: AtomicBoolean,
      blockGitoids: Set[String],
      cnt: AtomicInteger,
      runningCnt: AtomicInteger,
      totalStart: Instant,
      dead_? : AtomicBoolean,
      loopStart: Instant,
      writeThreadCnt: AtomicInteger
  ): Option[Thread] = {

    val storage = MemStorage(Some(destDir))

    // start time
    val start = Instant.now()

    val startedRunning = cnt.get()

    val batchName = destDir.getName()

    // fork `threadCnt` threads to do the work
    val threads = for { threadNum <- 0 until threadCnt } yield {
      val t = new Thread(
        () => {

          @tailrec
          def doPoll(): ToProcess = {
            val tried = queue.poll()
            if (tried != null || !stillWorking.get()) {
              tried
            } else {
              // wait 1-32 ms
              Thread.sleep(1 + Math.abs(Helpers.randomLong()) % 32)
              doPoll()
            }
          }

          // pull the files from the channel
          // if the channel is closed/empty, `None` will be
          // returned, handle it gracefully

          var toProcess: ToProcess = null
          while (
            (cnt.get() - startedRunning) < maxRecords // only run so many items
            &&
            !dead_?.get() && {
              toProcess = doPoll();
              toProcess
            } != null
          ) {
            val localStart = Instant.now()
            try {
              // build the package

              toProcess.process(
                None,
                store = storage,
                parentScope = ParentScope.forAndWith(toProcess.main, None),
                blockList = blockGitoids,
                keepRunning = () => !dead_?.get(),
                atEnd = (parent, _) => {
                  if (parent.isEmpty) {
                    val updatedCnt = cnt.addAndGet(1)
                    val theDuration = Duration
                      .between(localStart, Instant.now())
                    if (
                      theDuration.getSeconds() > 30 || updatedCnt % 1000 == 0
                    ) {
                      val now = Instant.now()
                      val totalDuration = Duration.between(totalStart, now)
                      val processDuration = Duration.between(loopStart, now)
                      val totalItems = runningCnt.get()
                      val avgMsg = if (processDuration.getSeconds() > 0) {
                        val itemsPerSecond =
                          updatedCnt.toDouble / processDuration
                            .getSeconds()
                            .toDouble
                        val itemsPerMinute = itemsPerSecond * 60.0d
                        val left = totalItems.toDouble - updatedCnt.toDouble
                        val remainingDuration = Duration.ZERO.plusSeconds(
                          (left / itemsPerSecond).round
                        )
                        f" Items/minute ${itemsPerMinute.round}, est remaining ${remainingDuration}"
                      } else ""
                      logger.info(
                        f"Processed ${updatedCnt} of ${totalItems} at ${totalDuration}/${processDuration}${avgMsg}. ${toProcess.main} took ${theDuration} vertices ${String
                            .format("%,d", storage.size())}"
                      )
                    }
                  }
                }
              )

            } catch {
              case ise: IllegalStateException => {
                logger.error(
                  f"Failed illegal state ${toProcess.main} -- ${toProcess.mimeType} ${ise}"
                )
                dead_?.set(true)
                throw ise
              }
              case ioe: IOException => {
                if (
                  ioe.getMessage() != null && ioe
                    .getMessage()
                    .indexOf("Too many open files") > 0
                ) {
                  dead_?.set(true)
                  throw ioe

                }
                logger.error(
                  f"Failed IO ${toProcess.main} ${toProcess.mimeType} ${ioe}"
                )
              }
              case e: Exception => {
                logger.error(
                  f"Failed generic ${toProcess.main} -- ${toProcess.mimeType} ${e}"
                )
                // Helpers.bailFail()
              }

            }

          }
        },
        f"gitoid ${batchName} ${threadNum}"
      )
      t.start()
      t
    }

    // wait for the threads to complete
    for { t <- threads } {
      // deal with SIGKILL/ctrl-C
      if (t.isInterrupted() || dead_?.get()) {
        Thread.currentThread().interrupt()
        throw InterruptedException(f"${t.getName()} was interrupted")
      }
      t.join()
    }

    if (!dead_?.get()) {
      val thread = new Thread(
        () => {
          try {
            logger.info(
              f"Finished processing ${cnt.get()}, vertices ${storage.size()} at ${Duration
                  .between(start, Instant.now())}"
            )

            val writeStart = Instant.now()

            val ret = storage match {
              case lf: (ListFileNames & Storage) if !dead_?.get() =>
                computeAndAddMerkleTrees(lf)
                writeGoatRodeoFiles(lf)
              case _ => logger.error("Didn't write"); None
            }

            logger.info(
              f"Total build time for ${batchName} ${Duration
                  .between(start, Instant.now())}, write took ${Duration
                  .between(writeStart, Instant.now())}"
            )
          } finally {
            writeThreadCnt.decrementAndGet()
          }
        },
        f"Saver ${batchName}"
      )
      writeThreadCnt.addAndGet(1)
      thread.start()
      Some(thread)
    } else None

  }

  /** Finds all the Items that "Contain" other items and generate a Merkle Tree
    * alias
    *
    * @param data
    *   the items
    */
  def computeAndAddMerkleTrees(data: ListFileNames & Storage): Unit = {
    val keys = data.keys()

    logger.info(f"Computing merkle trees for ${keys.size} items")
    val merkleTrees = for {
      k <- keys.toVector.par
      item <- data.read(k)
      if !item.connections.filter(a => EdgeType.isDown(a._1)).isEmpty
    } yield {
      item.identifier -> GitOIDUtils.merkleTreeFromGitoids(
        item.connections.toVector
          .filter(a => EdgeType.isDown(a._1))
          .map(_._2)
      )
    }

    logger.info(f"Computed ${merkleTrees.size} Merkle Trees")

    // write the aliases
    for { (itemId, tree) <- merkleTrees } {
      // create the Merkle Tree Alias
      data.write(
        tree,
        maybeAlias => {
          val alias = maybeAlias.getOrElse(
            Item(
              identifier = tree,
              // reference = Item.noopLocationReference,
              connections = TreeSet(),
              bodyMimeType = None,
              body = None
            )
          )
          val toAdd = (EdgeType.aliasTo, itemId)
          val updatedAlias =
            if (alias.connections.contains(toAdd)) { alias }
            else {
              alias.copy(
                connections = (alias.connections + toAdd)
              )
            }
          updatedAlias
        },
        item => f"Inserting Merkle Tree of ${itemId}"
      )
      // and update the Item with the AliasFrom
      data.write(
        itemId,
        {
          case None =>
            throw new Exception(
              f"Should be able to find ${itemId}, but it's missing"
            )
          case Some(item) =>
            val toAdd = (EdgeType.aliasFrom, tree)
            if (item.connections.contains(toAdd)) item
            else item.copy(connections = item.connections + toAdd)
        },
        item => f"Updating Merkle Tree alias for ${itemId}"
      )
    }

    logger.info(f"Wrote ${merkleTrees.size} Merkle Trees")

  }

  def writeGoatRodeoFiles(
      store: ListFileNames & Storage
  ): Option[File] = {
    store.target() match {
      case Some(target) => {
        logger.info(f"In store with target ${target}")
        target.mkdirs()
        val start = Instant.now()

        val purlOut = store.purls()

        logger.info(f"Writing ${purlOut.length} Package URLs")
        val purlFile = File(target, "purls.txt")
        purlFile.createNewFile()
        val bw = new BufferedWriter(new FileWriter(purlFile))
        try {
          for (purl <- purlOut) {
            bw.write(f"${purl.canonicalize()}\n")
          }
        } finally {
          bw.flush()
          bw.close()
        }
        logger.info("Wrote pURLs, about to sort the index")

        // make sure the destination exists
        target.getAbsoluteFile().mkdirs()
        val sorted = {
          val allItems = store
            .pathsSortedWithMD5()
            .flatMap(v => store.read(v._2))

          allItems
        }

        logger.info(
          f"Post-sort at ${Duration.between(start, Instant.now())}"
        )

        sorted.par.foreach(_.cachedCBOR)

        logger.info(
          f"Post CBOR cache at ${Duration.between(start, Instant.now())}"
        )

        val cnt = sorted.length

        val ret = GraphManager.writeEntries(
          target,
          sorted.iterator
        )

        logger.info(
          f"Wrote ${cnt} entries in ${Duration.between(start, Instant.now())}"
        )

        Some(ret._2)
      }
      case _ => None
    }

  }
}
