package dotty.tools.scaladoc.mdoc

// import mdoc.Reporter
import dotty.tools.scaladoc.mdoc.Document
import dotty.tools.scaladoc.mdoc._
// import scala.meta._
// import scala.meta.inputs.Input
// import scala.meta.inputs.Position
// import mdoc.internal.pos.PositionSyntax._
import dotty.tools.scaladoc.mdoc.DocumentBuilder
import dotty.tools.scaladoc.mdoc.MdocNonFatal
import dotty.tools.scaladoc.mdoc.CompatClassloader
import java.nio.file.Path
import java.nio.file.Paths

object MarkdownBuilder {

  def default(): MarkdownCompiler = fromClasspath(classpath = System.getProperty("java.class.path"), scalacOptions = "")

  def buildDocument(
      compiler: MarkdownCompiler,
      reporter: MdocReporter,
      sectionInputs: List[SectionInput],
      instrumented: Instrumented,
      filename: String
  ): EvaluatedDocument = {
    val instrumentedInput = InstrumentedInput(filename, instrumented.source)
    // reporter.debug(s"$filename: instrumented code\n$instrumented")
    val compileInput = Input(filename, instrumented.source)
    // val edit = SectionInput.tokenEdit(sectionInputs, compileInput)
    val compiled = compiler.compile(
      compileInput,
      reporter,
      // edit,
      "repl.MdocSession$",
      // instrumented.fileImports
    )
    val doc = compiled match {
      case Some(cls) =>
        val ctor = cls.getDeclaredConstructor()
        ctor.setAccessible(true)
        val doc = ctor.newInstance().asInstanceOf[DocumentBuilder].$doc
        try {
          doc.build(instrumentedInput)
        } catch {
          case e: DocumentException =>
            val index = e.sections.length - 1
            val input = sectionInputs(index).input
            val pos =
              if (e.pos.isEmpty) {
                -1
              } else {
                e.pos.startLine
                // val slice = Position.Range(
                //   input,
                //   e.pos.startLine,
                //   e.pos.startColumn,
                //   e.pos.endLine,
                //   e.pos.endColumn
                // )
                // slice.toUnslicedPosition
              }
            reporter.error(pos, e.getCause)
            Document(instrumentedInput, e.sections)
          case MdocNonFatal(e) =>
            reporter.error(e)
            Document.empty(instrumentedInput)
        }
      case None =>
        // An empty document will render as the original markdown
        Document.empty(instrumentedInput)
    }
    EvaluatedDocument(doc, sectionInputs)
  }

  def fromClasspath(classpath: String, scalacOptions: String): MarkdownCompiler = {
    val fullClasspath =
      if (classpath.isEmpty) defaultClasspath(_ => true)
      else {
        val base = Classpath(classpath)
        val runtime = defaultClasspath(path => {
          val pathString = path.toString
          pathString.contains("scala-library") ||
          pathString.contains("scala-reflect") ||
          pathString.contains("fansi") ||
          pathString.contains("pprint") ||
          pathString.contains("mdoc-interfaces") ||
          (pathString.contains("mdoc") && pathString.contains("runtime")) ||
          (pathString.contains("mdoc") && pathString.contains("printing"))
        })
        base ++ runtime
      }
    new MarkdownCompiler(fullClasspath.syntax, scalacOptions)
  }

  private def defaultClasspath(fn: Path => Boolean): Classpath = {
    val paths =
      CompatClassloader
        .getURLs(getClass.getClassLoader)
        .iterator
        .map(url => Paths.get(url.toURI))
        .filter(p => fn(p))
        .toList
    Classpath(paths)
  }

}
