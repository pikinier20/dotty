package dotty.tools.scaladoc.mdoc

import scala.util.matching.Regex
import scala.collection.mutable
import scala.util.Failure
import scala.util.Success

case class Instrumented(
    source: String
)

object Instrumented {
  def fromSource(
      source: String,
      reporter: MdocReporter
  ): Instrumented = {
    Instrumented(
      source
    )
  }
}
