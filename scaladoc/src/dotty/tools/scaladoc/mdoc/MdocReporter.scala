package dotty.tools.scaladoc.mdoc

import dotty.tools.scaladoc.{ DocContext, report, compilerContext }
import dotty.tools.dotc.core.Contexts._

class MdocReporter(using DocContext) {
  import dotty.tools.scaladoc.compilerContext

  private def mdocMessage(msg: String) = s"MDoc: $msg"

  def error(line: Int, m: String): Unit =
    report.error(mdocMessage(s"At line: $line. $m"))

  def warn(line: Int, m: String): Unit =
    report.warning(mdocMessage(s"At line: $line. $m"))

  def error(m: String): Unit =
    report.error(mdocMessage(m))

  def error(t: Throwable): Unit = report.error(mdocMessage(t.getMessage()))

  def error(line: Int, t: Throwable): Unit =
    report.error(mdocMessage(s"At line: $line. ${t.getMessage()}"))

  def warn(m: String): Unit =
    report.warning(mdocMessage(m))
}