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

import goatrodeo.util.GitOIDUtils
import java.util.regex.Pattern
import goatrodeo.util.Helpers
import java.io.ByteArrayInputStream
import goatrodeo.envelopes.MD5
import goatrodeo.envelopes.Position
import goatrodeo.envelopes.MultifilePosition
import io.bullet.borer.Cbor
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import goatrodeo.omnibor.MemStorage
import goatrodeo.omnibor.EdgeType
import goatrodeo.omnibor.ToProcess
import goatrodeo.omnibor.Builder
import goatrodeo.omnibor.GraphManager
import java.io.IOException
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.ByteArrayOutputStream
import java.io.OutputStreamWriter
import goatrodeo.util.FileWalker
import goatrodeo.util.FileWrapper
import java.io.BufferedInputStream
import goatrodeo.util.ArtifactWrapper
import goatrodeo.util.PURLHelpers.Ecosystems
import goatrodeo.omnibor.strategies.Debian
import com.github.packageurl.PackageURL

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {

  test("gitoid to file") {
    val test = List(
      "gitoid:blob:sha256:880485f48092dd308a2ad8a7b6ce060c4b2ec81ecb4ba3f5fd450b79136a852a",
      "blob:sha256:880485f48092dd308a2ad8a7b6ce060c4b2ec81ecb4ba3f5fd450b79136a852a",
      ":sha256:880485f48092dd308a2ad8a7b6ce060c4b2ec81ecb4ba3f5fd450b79136a852a",
      "880485f48092dd308a2ad8a7b6ce060c4b2ec81ecb4ba3f5fd450b79136a852a"
    )
    test.foreach(v =>
      assertEquals(
        GitOIDUtils.urlToFileName(v),
        (
          "880",
          "485",
          "f48092dd308a2ad8a7b6ce060c4b2ec81ecb4ba3f5fd450b79136a852a"
        )
      )
    )
  }

  test("regex") {
    val p = Pattern.compile("a")
    val m = p.matcher("aaaa")
    assert(m.find())
  }

  test("good hex for sha256") {
    val txt = Array[Byte](49, 50, 51, 10)
    val digest = GitOIDUtils.HashType.SHA256.getDigest()
    assertEquals(
      Helpers.toHex(digest.digest(txt)),
      "181210f8f9c779c26da1d9b2075bde0127302ee0e3fca38c9a83f5b1dd8e5d3b"
    )

    assertEquals(
      Helpers.toHex(Helpers.computeSHA256(new ByteArrayInputStream(txt))),
      "181210f8f9c779c26da1d9b2075bde0127302ee0e3fca38c9a83f5b1dd8e5d3b"
    )
  }

  test("long to hex and back again") {
    assertEquals(Helpers.toHex(0x1), "0000000000000001")

    assertEquals(Helpers.toHex(0x0030005000a00f01L), "0030005000a00f01")

    val txt = Array[Byte](49, 50, 51, 10)

    assertEquals(
      Helpers.toHex(
        Helpers.byteArrayToLong63Bits(
          Helpers.computeSHA256(new ByteArrayInputStream(txt))
        )
      ),
      "181210f8f9c779c2"
    )
  }

  test("File Type Detection") {
    assert({
      val name = "test_data/HP1973-Source.zip"
      FileWalker
        .withinArchiveStream(
          FileWrapper(File(name), name)
        ) { _ => 42 }
        .isDefined
    })
    assert({
      val name = "test_data/log4j-core-2.22.1.jar"
      FileWalker
        .withinArchiveStream(
          FileWrapper(File(name), name)
        ) { _ => 42 }
        .isDefined
    })

    assert({
      val name = "test_data/ics_test.tar"
      FileWalker
        .withinArchiveStream(FileWrapper(File(name), name)) { _ => 42 }
        .isDefined
    })

    assert({
      val name = "test_data/nested.tar"
      FileWalker
        .withinArchiveStream(FileWrapper(File(name), name)) { _ => 42 }
        .isDefined
    })

    assert({
      val name = "test_data/tk8.6_8.6.14-1build1_amd64.deb"
      FileWalker
        .withinArchiveStream(
          FileWrapper(File(name), name)
        ) { _ => 42 }
        .isDefined
    })
    assert({
      val name = "test_data/tk-8.6.13-r2.apk"
      FileWalker
        .withinArchiveStream(
          FileWrapper(File(name), name)
        ) { _ => 42 }
        .isDefined
    })

    assert({
      val name = "test_data/empty.tgz"
      FileWalker
        .withinArchiveStream(FileWrapper(File(name), name)) { _ => 42 }
        .isDefined
    })
    assert({
      val name = "test_data/toml-rs.tgz"
      FileWalker
        .withinArchiveStream(FileWrapper(File(name), name)) { _ => 42 }
        .isDefined
    })

  }

  test("Walk a tar file") {
    val name = "test_data/empty.tgz"
    val count =
      FileWalker
        .withinArchiveStream(FileWrapper(File(name), name)) { x => x.length }
        .get

    assert(count > 2)
  }

  test("deal with nesting") {
    val name = "test_data/nested.tar"
    val nested = FileWrapper(File(name), name)

    val store = ToProcess.buildGraphFromArtifactWrapper(nested)

    val gitoids = store.gitoidKeys()
    val cnt = gitoids.size

    assert(cnt > 1200, f"expected more than 1,200, got ${cnt}")
  }

  test("Compute pURL for .deb") {
    val name = "test_data/tk8.6_8.6.14-1build1_amd64.deb"
    val (maybePurl, attrs) = Debian
      .computePurl(
        FileWrapper(File(name), name)
      )
      .get
    assert(maybePurl.isDefined, "Should compute a purl")
    val purl = maybePurl.get
    assertEquals(purl.getName(), "tk8.6")
    assert(
      attrs.get("maintainer").get.size > 0,
      "Should have a mainter"
    )
    assert(
      attrs.get("description").get.head.value.contains("look-and-feel"),
      "The description must support multi-line"
    )
  }

  test("Compute pURL for another .deb") {
    val name = "test_data/libasound2_1.1.3-5ubuntu0.6_amd64.deb"

    val (maybePurl, attrs) = Debian
      .computePurl(
        FileWrapper(File(name), name)
      )
      .get
    assert(maybePurl.isDefined, "Should compute a purl")
    val purl = maybePurl.get
    assertEquals(purl.getName(), "libasound2")
    assert(
      attrs.get("maintainer").get.size > 0,
      "Should have a mainter"
    )
    assert(
      attrs
        .get("description")
        .get
        .head
        .value
        .contains("ALSA library and its standard plugins"),
      "The description must support multi-line"
    )

  }

  test("deal with .deb and zst") {
    val name = "test_data/tk8.6_8.6.14-1build1_amd64.deb"
    val nested =
      FileWrapper(File(name), name)

    val store = ToProcess.buildGraphFromArtifactWrapper(nested)
    val gitoids = store.gitoidKeys()
    val cnt = gitoids.size

    assert(cnt > 10, f"expected more than 10, got ${cnt}")
  }

  test("calculate mime type for class file") {
    val classFileName = "target/scala-3.6.3/classes/goatrodeo/Howdy.class"

    val f = new File(classFileName)
    val inputStream = new BufferedInputStream(new FileInputStream(f))
    val mimeType = ArtifactWrapper.mimeTypeFor(inputStream, classFileName)
    assert(
      mimeType == "application/java-vm",
      f"Expecting mime type for a class file to be 'application/java-vm' but got ${mimeType}"
    )
  }

  test("Build from nested") {
    val name = "test_data/nested.tar"
    val nested = FileWrapper(File(name), name)
    val store1 = ToProcess.buildGraphFromArtifactWrapper(nested)
    val store2 = ToProcess.buildGraphFromArtifactWrapper(
      nested,
      block = Set(
        "gitoid:blob:sha256:e3f8d493cb200fd95c4881e248148836628e0f06ddb3c28cb3f95cf784e2f8e4"
      )
    )

    val store3 = ToProcess.buildGraphFromArtifactWrapper(nested)

    assertEquals(
      store1.keys().toSet,
      store3.keys().toSet,
      "Builds are reproducable"
    )
    assertNotEquals(
      store1.keys().toSet,
      store2.keys().toSet,
      "Block list should work"
    )

    val gitoids = store1.gitoidKeys()

    assert(
      gitoids.size > 1200,
      f"Expection more than 1,200 items, got ${gitoids.size}"
    )
    assert(store1.size() > 2200)
    val keys = store1.keys()
    assert(!keys.filter(_.startsWith("sha256:")).isEmpty)
    assert(!keys.filter(_.startsWith("md5:")).isEmpty)
    assert(!keys.filter(_.startsWith("sha1:")).isEmpty)
    assert(keys.filter(_.startsWith("floof:")).isEmpty)
    val topAlias = store1
      .read(
        "sha256:82ceabe5192a5c3803f8b73536e83cd59e219fb560d8ed9e0c165728b199c0d7"
      )
      .get
    val gitoid = topAlias.connections.head._2
    assert(gitoid.startsWith("gitoid:"))
    val top = store1.read(gitoid).get
    store1.read("gitoid:blob:sha1:2e79b179ad18431600e9a074735f40cd54dde7f6").get
    for { edge <- top.connections if edge._1 == EdgeType.contains } {
      val contained = store1.read(edge._2).get
    }

    val log4j = store1
      .read(
        "gitoid:blob:sha256:e3f8d493cb200fd95c4881e248148836628e0f06ddb3c28cb3f95cf784e2f8e4"
      )
      .get
    assert(
      log4j.connections.filter(_._1 == EdgeType.contains).size > 1200
    )
  }

  test("Build from Java") {
    val source = File("test_data/jar_test")
    val strategy = ToProcess.strategyForDirectory(source, false)

    assert(
      strategy.length >= 2,
      f"Expecting at least 2 files, got ${strategy.length}"
    )

    var packages: Vector[PackageURL] = Vector()
    val store = ToProcess.buildGraphForToProcess(
      strategy,
      purlOut = purl => {
        packages = packages :+ purl
      }
    )

    val keys = store.keys()
    val items = keys.toVector.flatMap(store.read(_))
    assert(items.length > 1100)

    val sourceRef = items.filter(i =>
      i.connections.filter(e => EdgeType.isBuiltFrom(e._1)).size > 0
    )
    val fromSource = for {
      i <- items; c <- i.connections if c._1 == EdgeType.buildsTo
    } yield c
    assert(sourceRef.length > 100, "Expected to find source files")

    assert(fromSource.length == sourceRef.length)

    // the package URL is picked up
    val withPurl =
      items.filter(i => i.connections.filter(_._2.startsWith("pkg:")).size > 0)

    assert(
      withPurl.length == 6,
      f"expected 6, got ${packages.length} and ${packages}"
    )

    val withPurlSources = withPurl.filter(i =>
      i.connections.filter(_._2.endsWith("?packaging=sources")).size > 0
    )
    assert(withPurlSources.length == 2)

    val jars =
      withPurl.filter(i => i.connections.filter(_._2.contains("?")).size == 0)

    assert(jars.length == 2, f"Expecting two JARs, but got ${jars.length}")

    val extra = jars(0).body.get.extra

    assert(extra.get("manifest").isDefined)
    assert(extra.get("pom").isDefined)
    assert(extra.get("$$Sloth").isEmpty)
  }

  test("Unreadable JAR") {
    val source = File(File(System.getProperty("user.home")), "/tmp/repo_ea")

    // the test takes a couple of files with questionable TAR and ZIP archives
    // and ensures that they don't cause exceptions
    if (source.isDirectory()) {
      for {
        toTry <- Vector(
          "adif-processor-1.0.65.jar",
          "alpine-executable-war-1.2.2.jar"
        )
      } {
        val bad = File(source, toTry)
        val badWrapper = FileWrapper(bad, toTry)
        ToProcess.buildGraphFromArtifactWrapper(badWrapper)
        // No pURL
        // val pkgIndex = store.read("pkg:maven").get
        // assert(
        //   pkgIndex.connections.size > 0,
        //   f"We should have had at least one package, but only found ${pkgIndex.connections.size}"
        // )
      }

    }
  }

  test("Build lots of JARs") {
    val source = File(File(System.getProperty("user.home")), "/tmp/repo_ea")

    if (source.isDirectory()) {

      val resForBigTent = File("res_for_big_tent")

      // delete files if they exist
      if (resForBigTent.exists()) {
        if (resForBigTent.isDirectory()) {
          for { v <- resForBigTent.listFiles() } { v.delete() }
        } else {
          resForBigTent.delete()
        }
      }
      import scala.collection.JavaConverters.collectionAsScalaIterableConverter
      import scala.collection.JavaConverters.iterableAsScalaIterableConverter

      Builder.buildDB(source, resForBigTent, 32, None, 1000000)

      // no pURL
      // val pkgIndex = store.read("pkg:maven").get
      // assert(
      //   pkgIndex.connections.size > 4500,
      //   f"We should have had more than 100 packages, but only found ${pkgIndex.connections.size}"
      // )
    }
  }

}
