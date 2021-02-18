package dotty.tools.scaladoc.mdoc

import dotty.tools.scaladoc.mdoc.Document

case class EvaluatedDocument(
    instrumented: String,
    sections: List[EvaluatedSection]
)

object EvaluatedDocument {
  def apply(document: Document, trees: List[SectionInput]): EvaluatedDocument = {
    val instrumented = document.instrumented.text
    EvaluatedDocument(
      instrumented,
      document.sections.zip(trees).map { case (a, b) =>
        EvaluatedSection(a, b.input, b.source, b.stats, b.mod)(b.ctx)
      }
    )
  }
}
