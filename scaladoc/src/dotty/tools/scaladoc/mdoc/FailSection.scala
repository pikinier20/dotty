package dotty.tools.scaladoc.mdoc

case class FailSection(
    code: String,
    startLine: Int,
    startColumn: Int,
    endLine: Int,
    endColumn: Int
)
