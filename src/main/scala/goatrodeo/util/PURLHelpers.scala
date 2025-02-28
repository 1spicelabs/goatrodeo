package goatrodeo.util

import com.github.packageurl.PackageURL
import com.github.packageurl.PackageURLBuilder

/** Helpers related to Package URLs
  */
object PURLHelpers {

  /** Qualifiers for Maven packages
    */
  lazy val mavenQualifiers: Map[String, (String, String)] =
    Map(
      "pom" -> ("type", "pom"),
      "sources" -> ("packaging", "sources"),
      "javadoc" -> ("classifier", "javadoc")
    )

  /** Known ecosystems
    */
  enum Ecosystems {
    case Maven
    case Debian
  }

  /** Mapping between the Ecosystems and the text names and known qualifiers
    */
  lazy val ecosystems: Map[Ecosystems, (String, Some[Map[String, (String, String)]] | None.type)] = Map(
    Ecosystems.Maven -> ("maven", Some(mavenQualifiers)),
    Ecosystems.Debian -> ("deb", None)
  );

  /** Take a bunch of "information" and turn it into a package URL
    *
    * @param ecosystem
    *   the ecosystem
    * @param namespace
    *   the optional namespace (Maven uses this)
    * @param artifactId
    *   the artifact ID
    * @param version
    *   the artifact version
    * @param qualifierName
    *   the name of the qualifier that is mapped to the ecosystem-specific
    *   qualifier information
    * @param qualifiers
    *   any additional qualifiers
    * @return
    */
  def buildPackageURL(
      ecosystem: Ecosystems,
      namespace: Option[String] = None,
      artifactId: String,
      version: String,
      qualifierName: Option[String] = None,
      qualifiers: Seq[(String, String)] = Seq()
  ): PackageURL = {
    val (ecosystemText, ecosystemQualifiers) =
      ecosystems.get(ecosystem) match {
        case None => ("unknown", Map())
        case Some(name -> ecosystemQualifiers) =>
          (name, ecosystemQualifiers.getOrElse(Map()))
      }

    var purlBuilder = PackageURLBuilder.aPackageURL().withType(ecosystemText)

    purlBuilder = namespace.foldLeft(purlBuilder) { case (pb, namespace) =>
      pb.withNamespace(namespace)
    }
    purlBuilder = purlBuilder.withName(artifactId).withVersion(version)

    purlBuilder = qualifierName
      .flatMap(name => ecosystemQualifiers.get(name))
      .foldLeft(purlBuilder) { case (pb, (k, v)) => pb.withQualifier(k, v) }

    purlBuilder = qualifiers.foldLeft(purlBuilder) { case (pb, (k, v)) =>
      pb.withQualifier(k, v)
    }

    val ret: PackageURL = purlBuilder.build()

    return ret
  }

}
