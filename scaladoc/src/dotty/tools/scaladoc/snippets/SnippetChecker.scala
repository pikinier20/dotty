package dotty.tools.scaladoc
package snippets

import dotty.tools.scaladoc.DocContext
import java.nio.file.Paths
import java.io.File

import dotty.tools.io.AbstractFile
import dotty.tools.dotc.fromtasty.TastyFileUtil

class SnippetChecker(val classpath: String, val bootclasspath: String, val tastyFiles: Seq[File], isScalajs: Boolean):
  private val sep = System.getProperty("path.separator")
  private val cp = List(
    tastyFiles.map(_.getAbsolutePath()).map(AbstractFile.getFile(_)).flatMap(TastyFileUtil.getClassPath(_)).distinct.mkString(sep),
    classpath,
    bootclasspath
  ).mkString(sep)


  private val compiler: SnippetCompiler = SnippetCompiler(classpath = cp, scalacOptions = if isScalajs then "-scalajs" else "")

  // These constants were found empirically to make snippet compiler
  // report errors in the same position as main compiler.
  private val constantLineOffset = 3
  private val constantColumnOffset = 4

  def checkSnippet(
    snippet: String,
    data: Option[SnippetCompilerData],
    arg: SnippetCompilerArg,
    lineOffset: SnippetChecker.LineOffset
  ): Option[SnippetCompilationResult] = {
    if arg.flag != SCFlags.NoCompile then
      val wrapped = WrappedSnippet(
        snippet,
        data.map(_.packageName),
        data.fold(Nil)(_.classInfos),
        data.map(_.imports).getOrElse(Nil),
        lineOffset + data.fold(0)(_.position.line) + constantLineOffset,
        data.fold(0)(_.position.column) + constantColumnOffset
      )
      val res = compiler.compile(wrapped, arg)
      Some(res)
    else None
  }

object SnippetChecker:
  type LineOffset = Int
  type SnippetCheckingFunc = (String, LineOffset, Option[SCFlags]) => Option[SnippetCompilationResult]
