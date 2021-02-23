package dotty.tools.scaladoc.mdoc

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import dotty.tools.scaladoc.mdoc.CompileResult
import dotty.tools.scaladoc.mdoc.CrashResult
import dotty.tools.scaladoc.mdoc.CrashResult.Crashed
import dotty.tools.scaladoc.mdoc.RangePosition
import dotty.tools.scaladoc.mdoc.FailSection
import dotty.tools.scaladoc.mdoc.MdocExceptions
import dotty.tools.scaladoc.DocContext

object Renderer {
  def render(
      code: String,
      origin: String,
      printer: Variable => String
  )(using ctx: DocContext): String = {
    val reporter = MdocReporter(origin)
    val compiler = MarkdownBuilder.fromClasspath(System.getProperty("java.class.path"), "")
    val sectionInput = SectionInput(code, Modifier.Default())
    val instrumented = Instrumenter.instrument(
      sectionInput,
      reporter,
      origin
    )
    val doc =
      MarkdownBuilder.buildDocument(
        compiler,
        reporter,
        sectionInput,
        instrumented,
        origin
      )
    s"""
      |${Renderer.renderEvaluatedSection(doc, doc.section, reporter, printer, compiler)}
      """.stripMargin

  }

  def renderCrashSection(
      section: EvaluatedSection,
      reporter: MdocReporter,
  ): String = {
    // require(section.mod.isCrash, section.mod)
    val out = new ByteArrayOutputStream()
    val ps = new PrintStream(out)
    ps.println("```scala")
    // ps.println(section.source.pos.text)
    val crashes = for {
      statement <- section.section.statements
      binder <- statement.binders
      if binder.value.isInstanceOf[Crashed]
    } yield binder.value.asInstanceOf[Crashed]
    crashes.headOption match {
      case Some(CrashResult.Crashed(e, _)) =>
        MdocExceptions.trimStacktrace(e)
        val stacktrace = new ByteArrayOutputStream()
        e.printStackTrace(new PrintStream(stacktrace))
        appendFreshMultiline(ps, stacktrace.toString())
        ps.append('\n')
      case None =>
        reporter.error("Expected runtime exception but program completed successfully")
    }
    ps.println("```")
    out.toString()
  }

  def appendMultiline(sb: PrintStream, string: String): Unit = {
    appendMultiline(sb, string, string.length)
  }

  def appendMultiline(sb: PrintStream, string: String, N: Int): Unit = {
    var i = 0
    while (i < N) {
      string.charAt(i) match {
        case '\n' =>
          sb.append("\n// ")
        case ch =>
          sb.append(ch)
      }
      i += 1
    }
  }

  def appendFreshMultiline(sb: PrintStream, string: String): Unit = {
    val N = string.length - (if (string.endsWith("\n")) 1 else 0)
    sb.append("// ")
    appendMultiline(sb, string, N)
  }

  def renderEvaluatedSection(
      doc: EvaluatedDocument,
      section: EvaluatedSection,
      reporter: MdocReporter,
      printer: Variable => String,
      compiler: MarkdownCompiler
  ): String = {
    val baos = new ByteArrayOutputStream()
    val sb = new PrintStream(baos)
    val statsWithSourcePos = section.statsWithSourcePos
    val input = section.sourcePos.linesSlice.mkString
    val totalStats = section.stats.length
    if (section.mod.isFailOrWarn) {
      sb.print(section.input)
    }
    section.section.statements.zip(section.statsWithSourcePos).zipWithIndex.foreach {
      case ((statement, (tree, sourcePos)), statementIndex) =>
        val pos = sourcePos
        val statementTxt = sourcePos.linesSlice.mkString.stripSuffix("\n").stripPrefix(" " * 2)
        if (!section.mod.isFailOrWarn) {
          sb.append(statementTxt)
        }
        if (statement.out.nonEmpty) {
          sb.append("\n")
          appendFreshMultiline(sb, statement.out)
        }
        val N = statement.binders.length
        statement.binders.zipWithIndex.foreach { case (binder, i) =>
          section.mod match {
            case Modifier.Fail() | Modifier.Warn() =>
              sb.append('\n')
              binder.value match {
                case FailSection(instrumented, startLine, startColumn, endLine, endColumn) =>
                  // TODO: Handle intended failure
                  ???
                case _ =>
                  //TODO: Handle unintended success
                  ???
              }
            case _ =>
              val pos = binder.pos
              val variable = new Variable(
                binder.name,
                binder.tpe.render,
                binder.value,
                pos,
                i,
                N,
                statementIndex,
                0,
                section.mod
              )
              sb.append(printer(variable))
          }
        }
      sb.append("\n")
    }
    baos.toString.trim
  }

}
