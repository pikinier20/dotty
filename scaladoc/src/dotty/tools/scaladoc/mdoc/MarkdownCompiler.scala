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

import dotty.tools.scaladoc.mdoc.Document
import dotty.tools.scaladoc.mdoc._
import dotty.tools.scaladoc.mdoc.DocumentBuilder
import dotty.tools.scaladoc.mdoc.MdocNonFatal
import dotty.tools.scaladoc.mdoc.CompatClassloader

import scala.collection.JavaConverters._
import scala.collection.Seq

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
      context: Context
  ): Unit = {
    clearTarget()
    val compiler = new Compiler
    val run = compiler.newRun(using context)
    val inputs = List(input)
    scala.util.Try(run.compileSources(inputs.map(toSource)))
    report(vreporter, input, run.runContext)
  }

  def compile(
      input: Input,
      vreporter: MdocReporter,
      className: String,
      retry: Int = 0
  ): Option[Class[_]] = {
    reset()
    val compilerReporter = new StoreReporter
    val context = driver.currentCtx.fresh
      .setSetting(
        driver.currentCtx.settings.outputDir,
        target
      )
      .setReporter(compilerReporter)
    compileSources(
      input,
      vreporter,
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
              className,
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

  private def nullableMessage(msgOrNull: String): String =
    if (msgOrNull == null) "" else msgOrNull

  private def report(
      vreporter: MdocReporter,
      input: Input,
      context: Context,
  ): Unit = {
    val infos = context.reporter.allErrors.toSeq.sortBy(_.pos.source.path)
    val message = infos.map {
      case diagnostic if diagnostic.position.isPresent =>
        val pos = diagnostic.position.get
        val msg = nullableMessage(diagnostic.message)
        val sourceLine = input.content.split("\n")(pos.startLine)
        s"\t$sourceLine\n\t$msg\n"
    }.mkString("\n")
    if !message.isEmpty then reportMessage(vreporter, message)
  }

  private def reportMessage(
      vreporter: MdocReporter,
      message: String
  ): Unit = vreporter.error(message)

  private def formatMessage(message: String): String = message

 }
