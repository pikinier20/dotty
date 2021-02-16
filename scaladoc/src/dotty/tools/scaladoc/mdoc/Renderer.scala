package dotty.tools.scaladoc.mdoc

import java.io.ByteArrayOutputStream
import java.io.PrintStream
// import mdoc.Reporter
// import mdoc.Variable
import dotty.tools.scaladoc.mdoc.CompileResult
import dotty.tools.scaladoc.mdoc.CrashResult
import dotty.tools.scaladoc.mdoc.CrashResult.Crashed
import dotty.tools.scaladoc.mdoc.RangePosition
import dotty.tools.scaladoc.mdoc.FailSection
import dotty.tools.scaladoc.mdoc.MdocExceptions
import dotty.tools.scaladoc.DocContext
// import mdoc.internal.pos.PositionSyntax._
// import mdoc.internal.pos.TokenEditDistance
// import scala.meta._
// import scala.meta.inputs.Position
// import mdoc.internal.cli.InputFile
// import mdoc.internal.cli.Settings

object Renderer {
  def render(
      file: Input,
      sections: List[Input],
      compiler: MarkdownCompiler,
      // settings: Settings,
      // reporter: Reporter,
      filename: String,
      printer: Variable => String
  )(using DocContext): String = {
    val reporter = MdocReporter()
    val inputs =
      sections.map(s => SectionInput(s, Modifier.Default()))
    val instrumented = Instrumenter.instrument(
      file,
      inputs,
      // settings,
      reporter
    )
    val doc =
      MarkdownBuilder.buildDocument(
        compiler,
        reporter,
        inputs,
        instrumented,
        filename
      )
    doc.sections
      .map(s => s"""```scala
                   |${Renderer.renderEvaluatedSection(doc, s, reporter, printer, compiler)}
                   |```""".stripMargin)
      .mkString("\n")
  }

  def renderCrashSection(
      section: EvaluatedSection,
      reporter: MdocReporter,
      // edit: TokenEditDistance
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
    if (/*section.mod.isFailOrWarn*/false) {
      sb.print(section.input)
    }
    section.section.statements.zip(section.statsWithSourcePos).zipWithIndex.foreach {
      case ((statement, (tree, sourcePos)), statementIndex) =>
        val pos = sourcePos
        //Source code has default indent due to body wrapping
        // val leadingTrivia = if statementIndex == 0 then "" else input.slice(
        //   statsWithSourcePos(statementIndex - 1)(1).end,
        //   pos.start
        // ).stripPrefix(" " * 2)
        // println(leadingTrivia)

        // if (/*!section.mod.isFailOrWarn*/true) {
        //   sb.append(leadingTrivia)
        // }
        val statementTxt = sourcePos.linesSlice.mkString.stripSuffix("\n").stripPrefix(" " * 2)
        if (/*!section.mod.isFailOrWarn*/true) {
          sb.append(statementTxt)
        }
        if (statement.out.nonEmpty) {
          sb.append("\n")
          appendFreshMultiline(sb, statement.out)
        }
        val N = statement.binders.length
        statement.binders.zipWithIndex.foreach { case (binder, i) =>
          section.mod match {
            // case Modifier.Fail() | Modifier.Warn() =>
            //   sb.append('\n')
            //   binder.value match {
            //     // case FailSection(instrumented, startLine, startColumn, endLine, endColumn) =>
            //     //   val compiled = compiler.fail(
            //     //     doc.sections.map(_.source),
            //     //     Input.String(instrumented),
            //     //     section.source.pos
            //     //   )
            //     //   val tpos = new RangePosition(startLine, startColumn, endLine, endColumn)
            //     //   val pos = tpos.toMeta(section)
            //     //   if (section.mod.isWarn && compiler.hasErrors) {
            //     //     // reporter.error(
            //     //     //   pos,
            //     //     //   s"Expected compile warnings but program failed to compile"
            //     //     // )
            //     //   } else if (section.mod.isWarn && !compiler.hasWarnings) {
            //     //     // reporter.error(
            //     //     //   pos,
            //     //     //   s"Expected compile warnings but program compiled successfully without warnings"
            //     //     // )
            //     //   } else if (section.mod.isFail && !compiler.hasErrors) {
            //     //     // reporter.error(
            //     //     //   pos,
            //     //     //   s"Expected compile errors but program compiled successfully without errors"
            //     //     // )
            //     //   }
            //     //   appendFreshMultiline(sb, compiled)
            //     case _ =>
            //       val obtained = pprint.PPrinter.BlackWhite.apply(binder).toString()
            //       throw new IllegalArgumentException(
            //         s"Expected FailSection. Obtained $obtained"
            //       )
            //   }
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
