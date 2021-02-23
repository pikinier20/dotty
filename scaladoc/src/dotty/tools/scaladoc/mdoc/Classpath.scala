package dotty.tools.scaladoc.mdoc

import java.io.File.pathSeparator
import java.nio.file.{ Path, Paths }

final case class Classpath(entries: List[Path]) {

  def ++(other: Classpath): Classpath =
    Classpath(entries ++ other.entries)

  @deprecated("Use .entries instead", "4.0.0")
  def shallow: List[Path] = entries

  def syntax: String = entries.mkString(pathSeparator)
  def structure: String = s"""Classpath("$syntax")"""
  override def toString: String = syntax
}

object Classpath {
  def apply(entry: Path): Classpath = {
    new Classpath(List(entry))
  }

  def apply(value: String): Classpath = {
    apply(Paths.get(value))
  }
}