package dotty.tools.scaladoc.mdoc

case class SourceStatement[T](value: T, source: String)
object SourceStatement extends StatementMacro
