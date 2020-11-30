package dotty.dokka

import org.jetbrains.dokka.base.resolvers.local._
import org.jetbrains.dokka.base.DokkaBase
import org.jetbrains.dokka.base.resolvers.external._
import org.jetbrains.dokka.base.resolvers.shared._
import org.jetbrains.dokka.base.resolvers.anchors._
import org.jetbrains.dokka.links.DRI
import org.jetbrains.dokka.model.DisplaySourceSet
import org.jetbrains.dokka.pages.RootPageNode
import org.jetbrains.dokka.plugability._
import collection.JavaConverters._
import java.util.{Set => JSet}

class ScalaExternalLocationProviderFactory(ctx: DokkaContext) extends ExternalLocationProviderFactory:
  override def getExternalLocationProvider(doc: ExternalDocumentation): ExternalLocationProvider =
    if doc.getPackageList.getLinkFormat == RecognizedLinkFormat.DokkaHtml then ScalaExternalLocationProvider(doc, ".html",ctx)
    else ???