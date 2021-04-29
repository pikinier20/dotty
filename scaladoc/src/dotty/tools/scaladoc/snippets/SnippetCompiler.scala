package dotty.tools.scaladoc
package snippets

import dotty.tools.io.{AbstractFile, VirtualDirectory}
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveCompiler
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.config.Settings.Setting._
import dotty.tools.dotc.interfaces.SourcePosition
import dotty.tools.dotc.ast.Trees.Tree
import dotty.tools.dotc.interfaces.{SourceFile => ISourceFile}
import dotty.tools.dotc.reporting.{ Diagnostic, StoreReporter }
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.{ Compiler, Run }
import dotty.tools.io.{AbstractFile, VirtualDirectory}
import dotty.tools.repl.AbstractFileClassLoader
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.interfaces.Diagnostic._

import scala.util.{ Try, Success, Failure }

class SnippetCompiler(
  classpath: String,
  val scalacOptions: String = "",
  target: AbstractFile = new VirtualDirectory("(memory)")
):

  private def newDriver: InteractiveDriver = {
    val defaultFlags =
      List("-color:never", "-unchecked", "-deprecation", "-Ximport-suggestion-timeout", "0")
    val options = scalacOptions.split("\\s+").toList
    val settings =
      options ::: defaultFlags ::: "-classpath" :: classpath :: Nil
    new InteractiveDriver(settings)
  }

  private val driver = newDriver

  private val scala3Compiler = new Compiler

  private def newRun(using ctx: Context): Run = scala3Compiler.newRun

  private def nullableMessage(msgOrNull: String): String =
    if (msgOrNull == null) "" else msgOrNull

  private def createReportMessage(wrappedSnippet: WrappedSnippet, arg: SnippetCompilerArg, diagnostics: Seq[Diagnostic]): Seq[SnippetCompilerMessage] = {
    val line = wrappedSnippet.outerLineOffset
    val column = wrappedSnippet.outerColumnOffset
    val innerLineOffset = wrappedSnippet.innerLineOffset
    val innerColumnOffset = wrappedSnippet.innerColumnOffset
    val infos = diagnostics.toSeq.sortBy(_.pos.source.path)
    val errorMessages = infos.map {
      case diagnostic if diagnostic.position.isPresent =>
        val diagPos = diagnostic.position.get
        val pos = Some(
          Position(diagPos.line + line - innerLineOffset, diagPos.column + column - innerColumnOffset, diagPos.lineContent, if arg.debug then diagPos.line else diagPos.line - innerLineOffset)
        )
        val dmsg = Try(diagnostic.message) match {
          case Success(msg) => msg
          case Failure(ex) => ex.getMessage
        }
        val msg = nullableMessage(dmsg)
        val level = MessageLevel.fromOrdinal(diagnostic.level)
        SnippetCompilerMessage(pos, msg, level)
      case d =>
        val level = MessageLevel.fromOrdinal(d.level)
        SnippetCompilerMessage(None, nullableMessage(d.message), level)
    }
    errorMessages
  }

  private def additionalMessages(wrappedSnippet: WrappedSnippet, arg: SnippetCompilerArg, context: Context): Seq[SnippetCompilerMessage] = {
      Option.when(arg.flag == SCFlags.Fail && !context.reporter.hasErrors)(
        SnippetCompilerMessage(None, "Snippet should not compile but compiled succesfully", MessageLevel.Error)
      ).toList
  }

  private def isSuccessful(arg: SnippetCompilerArg, context: Context): Boolean = {
    if arg.flag == SCFlags.Fail then context.reporter.hasErrors
    else !context.reporter.hasErrors
  }

  def compile(
    wrappedSnippet: WrappedSnippet,
    arg: SnippetCompilerArg
  ): SnippetCompilationResult = {
    val context = driver.currentCtx.fresh
      .setSetting(
        driver.currentCtx.settings.outputDir,
        target
      )
      .setReporter(new StoreReporter)
    val run = newRun(using context)
    run.compileFromStrings(List(wrappedSnippet.snippet))

    val messages =
      createReportMessage(wrappedSnippet, arg, context.reporter.pendingMessages(using context)) ++
      additionalMessages(wrappedSnippet, arg, context)

    val t = Option.when(!context.reporter.hasErrors)(target)
    SnippetCompilationResult(wrappedSnippet, isSuccessful(arg, context), t, messages)
  }
