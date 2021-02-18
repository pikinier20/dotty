package dotty.tools.scaladoc.mdoc

import org.junit.Test
import org.junit.Assert._
import dotty.tools.scaladoc.{ ScaladocTest, DocContext }

class BasicMdocTest extends ScaladocTest("objectSignatures"):
  override def runTest: Unit = afterRendering {
      val input = Input(
        "test",
        """
        |1+1
        |2+2
        |case class B(val a: String)
        |val a = BB("asd")
        |println(a.a)
        """.stripMargin
      )
      val rendered = Renderer.render(
        // input,
        List(input),
        "test",
        ReplVariablePrinter
      )
      println(rendered)
    }