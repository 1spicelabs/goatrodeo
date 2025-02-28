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

package goatrodeo

import com.typesafe.scalalogging.Logger
import goatrodeo.omnibor.Builder
import goatrodeo.util.Helpers
import org.apache.commons.io.filefilter.WildcardFileFilter
import scopt.OParser
import scopt.OParserBuilder

import java.io.File
import java.io.FileFilter
import scala.jdk.CollectionConverters._

/** The `main` class
  */
object Howdy {
  private val logger = Logger(getClass())

  /** Command Line Configuration
    *
    * @param analyze
    *   -- analyze a file against the GC (GitOID Corpus)
    * @param out
    *   -- the place to output either the analysis or the results of the build
    * @param build
    *   -- build a GitOID Corpus based on JAR files found in the directory
    * @param threads
    *   -- the number of threads for the build -- default 4... typically 4x the
    *   number of physical CPUs
    */
  case class Config(
      out: Option[File] = None,
      build: Option[File] = None,
      threads: Int = 4,
      blockList: Option[File] = None,
      maxRecords: Int = 50000
  )

  lazy val builder: OParserBuilder[Config] = OParser.builder[Config]
  lazy val parser1: OParser[Unit, Config] = {
    import builder._
    OParser.sequence(
      programName("goatrodeo"),
      head("goatrodeo", "0.6.2"),
      opt[File]("block").action((x, c) => c.copy(blockList = Some(x))),
      opt[File]('b', "build")
        .text("Build gitoid database from jar files in a directory")
        .action((x, c) =>
          c.copy(build = Some(x).filter(f => f.exists() && f.isDirectory()))
        ),
      opt[Int]("maxrecords")
        .text(
          "The maximum number of records to process at once. Default 50,000"
        )
        .action((x, c) => if (x > 100) c.copy(maxRecords = x) else c),
      opt[File]('o', "out")
        .text("output directory for the file-system based gitoid storage")
        .action((x, c) => c.copy(out = Some(x))),
      opt[Int]('t', "threads")
        .text(
          "How many threads to run (default 4). Should be 2x-3x number of cores"
        )
        .action((t, c) => c.copy(threads = t))
    )
  }

  private object ExpandFiles {
    def unapply(in: Option[Vector[File]]): Option[Vector[File]] =
      in match {
        case None => None
        case Some(vf) =>
          val ret = for {
            fa <- vf
            f = fixTilde(fa)
            i <- {
              val parent = f.getAbsoluteFile().getParentFile()
              val wcf: FileFilter = WildcardFileFilter.builder.setWildcards(f.getName()).get()
              parent.listFiles(wcf)
            }
          } yield {
            i
          }
          Some(ret)
      }

    def fixTilde(in: File): File = {
      if (in.getPath().startsWith("~" + File.separator)) {
        val path = System.getProperty("user.home") + in.getPath().substring(1)
        new File(path)
      } else in
    }
  }

  /** The entrypoint
    *
    * @param args
    *   an array of command line paramets
    */
  def main(args: Array[String]): Unit = {
    // parse the CLI params
    val parsed = OParser.parse(parser1, args, Config())

    // Based on the CLI parse, make the right choices and do the right thing
    parsed match {
      case Some(
            Config(Some(out), Some(buildFrom), threads, block, maxRecords)
          ) =>
        Builder.buildDB(
          buildFrom,
          out,
          threads,
          block,
          maxRecords
        )

      case Some(
            Config(out, Some(buildFrom), threads, _, _)
          ) =>
        logger.error(
          "`out`  must be defined... where does the build result go?"
        )
        println(OParser.usage(parser1))
        Helpers.bailFail()

      case _ => Helpers.bailFail()
    }
  }
}
