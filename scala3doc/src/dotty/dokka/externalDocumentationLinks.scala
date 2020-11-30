package dotty.dokka

import org.jetbrains.dokka._
import org.jetbrains.dokka.ConfigurationKt.{ExternalDocumentationLink => dokkaApplyFun}
import java.net.URL

case class ScaladocExternalDocumentationLink(val url: URL, val packages: List[String]) extends DokkaConfiguration.ExternalDocumentationLink:
  override def getPackageListUrl: URL = null
  override def getUrl: URL = url


object ExternalDocumentationLink:
  def apply(url: String, packageListUrl: Option[String] = None) =
    dokkaApplyFun(new URL(url), packageListUrl.fold(null)(new URL(_)))

  def scaladoc(url: String, packages: List[String]) =
    ScaladocExternalDocumentationLink(new URL(url), packages)