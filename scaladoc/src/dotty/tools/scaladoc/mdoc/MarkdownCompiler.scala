package dotty.tools.scaladoc.mdoc

import java.io.ByteArrayOutputStream
import java.io.File
import java.io.PrintStream
import java.net.URL
import java.net.URI
import java.net.URLClassLoader
import java.nio.file.Path
import java.nio.file.Paths
import sun.misc.Unsafe

// import mdoc.Reporter
import dotty.tools.scaladoc.mdoc.Document
import dotty.tools.scaladoc.mdoc._
import dotty.tools.scaladoc.mdoc.DocumentBuilder
import dotty.tools.scaladoc.mdoc.MdocNonFatal
// import mdoc.internal.pos.TokenEditDistance
import dotty.tools.scaladoc.mdoc.CompatClassloader
// import mdoc.internal.pos.PositionSyntax._

import scala.collection.JavaConverters._
import scala.collection.Seq
// import scala.meta.Classpath
// import scala.meta.AbsolutePath
// import scala.meta.inputs.Input
// import scala.meta.inputs.Position
// import scala.meta.internal.inputs.XtensionInputSyntaxStructure

import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveCompiler
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.config.Settings.Setting._
import dotty.tools.dotc.interfaces.SourcePosition
import dotty.tools.dotc.ast.Trees.Tree
import dotty.tools.dotc.interfaces.{SourceFile => ISourceFile}
import dotty.tools.dotc.reporting.Diagnostic
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.Compiler
import dotty.tools.io.{AbstractFile, VirtualDirectory}
import dotty.tools.repl.AbstractFileClassLoader
import dotty.tools.dotc.util.SourceFile

import scala.annotation.implicitNotFound

class MarkdownCompiler(
    classpath: String,
    val scalacOptions: String,
    target: AbstractFile = new VirtualDirectory("(memory)")
) {

  private def newDriver: InteractiveDriver = {
    val defaultFlags =
      List("-color:never", "-unchecked", "-deprecation", "-Ximport-suggestion-timeout", "0")
    val options = scalacOptions.split("\\s+").toList
    val settings =
      options ::: defaultFlags ::: "-classpath" :: classpath :: Nil
    new InteractiveDriver(settings)
  }
  private var driver = newDriver

  def shutdown(): Unit = {}

  def classpathEntries: Seq[Path] =
    driver.currentCtx.settings.classpath
      .value(using driver.currentCtx)
      .split(File.pathSeparator)
      .map(url => Paths.get(url))

  private def reset(): Unit = {
    driver = newDriver
  }
  private val appClasspath: Array[URL] = classpath
    .split(File.pathSeparator)
    .map(path => new File(path).toURI.toURL)
  private val appClassLoader = new URLClassLoader(
    appClasspath,
    this.getClass.getClassLoader
  )

  private def clearTarget(): Unit = target match {
    case vdir: VirtualDirectory => vdir.clear()
    case _ =>
  }

  private def toSource(input: Input): SourceFile = {
    SourceFile.virtual(input.name, input.content)
  }

  def hasErrors: Boolean = driver.currentCtx.reporter.hasErrors
  def hasWarnings: Boolean = driver.currentCtx.reporter.hasWarnings

  def compileSources(
      input: Input,
      vreporter: MdocReporter,
      // edit: TokenEditDistance,
      // fileImports: List[FileImport],
      context: Context
  ): Unit = {
    clearTarget()
    val compiler = new Compiler
    val run = compiler.newRun(using context)
    val inputs = List(input)
    scala.util.Try(run.compileSources(inputs.map(toSource)))
    report(vreporter, input, /*fileImports,*/ run.runContext /*, edit*/)
  }

  def compile(
      input: Input,
      vreporter: MdocReporter,
      // edit: TokenEditDistance,
      className: String,
      // fileImports: List[FileImport],
      retry: Int = 0
  ): Option[Class[_]] = {
    reset()
    val context = driver.currentCtx.fresh.setSetting(
      driver.currentCtx.settings.outputDir,
      target
    )
    compileSources(
      input,
      vreporter,
      // edit,
      // fileImports,
      context
    )
    if (!context.reporter.hasErrors) {
      val loader = new AbstractFileClassLoader(target, appClassLoader)
      try {
        Some(loader.loadClass(className))
      } catch {
        case _: ClassNotFoundException =>
          if (retry < 1) {
            reset()
            compile(
              input,
              vreporter,
              // edit,
              className,
              // fileImports,
              retry + 1
            )
          } else {
            vreporter.error(
              s"${input.content}: skipping file, the compiler produced no classfiles " +
                "and reported no errors to explain what went wrong during compilation. " +
                "Please report an issue to https://github.com/scalameta/mdoc/issues."
            )
            None
          }
      }
    } else {
      None
    }
  }

  // private def toMetaPosition(
  //     edit: TokenEditDistance,
  //     position: SourcePosition
  // ): Position = {
  //   def toOffsetPosition(offset: Int): Position = {
  //     edit.toOriginal(offset) match {
  //       case Left(_) =>
  //         Position.None
  //       case Right(p) =>
  //         p.toUnslicedPosition
  //     }
  //   }
  //   (edit.toOriginal(position.start), edit.toOriginal(position.end - 1)) match {
  //     case (Right(start), Right(end)) =>
  //       Position.Range(start.input, start.start, end.end).toUnslicedPosition
  //     case (_, _) =>
  //       toOffsetPosition(position.point - 1)
  //   }
  // }

  private def nullableMessage(msgOrNull: String): String =
    if (msgOrNull == null) "" else msgOrNull

  private def report(
      vreporter: MdocReporter,
      input: Input,
      // fileImports: List[FileImport],
      context: Context,
      // edit: TokenEditDistance
  ): Unit = {
    val infos = context.reporter.allErrors.toSeq.sortBy(_.pos.source.path)
    infos.foreach {
      case diagnostic if diagnostic.position.isPresent =>
        val pos = diagnostic.position.get
        val msg = nullableMessage(diagnostic.message)
        // val mpos = toMetaPosition(edit, pos)
        val actualMessage = msg
          // if (mpos == Position.None) {
          //   val line = pos.lineContent
          //   if (line.nonEmpty) {
          //     formatMessage(pos, msg)
          //   } else {
          //     msg
          //   }
          // } else {
          //   msg
          // }
        reportMessage(vreporter, diagnostic, /*mpos,*/ actualMessage)
      case _ =>
    }
  }

  private def reportMessage(
      vreporter: MdocReporter,
      diagnostic: Diagnostic,
      // mpos: Position,
      message: String
  ): Unit = diagnostic match {
    case _: Diagnostic.Error => vreporter.error(message)
    // case _: Diagnostic.Info => MdocReporter.info(message)
    case _: Diagnostic.Warning => vreporter.warn(message)
    case _ =>
  }
  private def formatMessage(/*pos: SourcePosition,*/ message: String): String = message
    // new CodeBuilder()
    //   .println(s"${pos.source().path()}:${pos.line + 1} (mdoc generated code) $message")
    //   .println(pos.lineContent)
    //   .println(pos.point().toString)
    //   .toString

 }
