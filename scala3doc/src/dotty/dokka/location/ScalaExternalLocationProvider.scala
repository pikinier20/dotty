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


class ScalaExternalLocationProvider(
  externalDocumentation: ExternalDocumentation,
  extension: String,
  ctx: DokkaContext
) extends DefaultExternalLocationProvider(externalDocumentation, extension, ctx):
  override def resolve(dri: DRI): String = super.resolve(dri)
