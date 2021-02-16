package dotty.tools.scaladoc.mdoc

import dotty.tools.scaladoc.mdoc.Section
import dotty.tools.dotc.ast.untpd.Tree
import dotty.tools.dotc.core.Contexts.Context

// Can be removed once the project is updated to use Scalameta parser for Scala 3
case class EvaluatedSection(section: Section, input: Input, root: Tree, stats: List[Tree], mod: Modifier)(implicit private val ctx: Context) {
  def out: String = section.statements.map(_.out).mkString

  def sourcePos = root.sourcePos

  def statsWithSourcePos = stats.map(s => s -> s.sourcePos)
}
