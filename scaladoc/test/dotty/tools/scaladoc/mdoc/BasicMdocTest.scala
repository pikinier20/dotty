package dotty.tools.scaladoc.mdoc

import org.junit.Test
import org.junit.Assert._
import dotty.tools.scaladoc.{ ScaladocTest, DocContext }

class BasicMdocTest extends ScaladocTest("objectSignatures"):
  override def runTest: Unit = afterRendering {
      val input = Input(
        "test",
        "1+1\n"+"2+2\n"+"case class B(val a: String)\n"+"val a = BB(\"asd\")\n"+"println(a.a)\n"
      )
      val rendered = Renderer.render(
        input,
        List(input),
        MarkdownBuilder.default(),
        "test",
        ReplVariablePrinter
      )
      println(rendered)
    }