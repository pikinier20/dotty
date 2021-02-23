package dotty.tools.scaladoc.mdoc

import dotty.tools.scaladoc.mdoc.Document

case class EvaluatedDocument(
    instrumented: String,
    section: EvaluatedSection
)

object EvaluatedDocument {
  def apply(document: Document, tree: SectionInput): EvaluatedDocument = {
    val instrumented = document.instrumented.text
    EvaluatedDocument(
      instrumented,
      (document.sections.head, tree) match {
         case (a, b)=> EvaluatedSection(a, b.input, b.source, b.stats, b.mod)(b.ctx)
      }
    )
  }
}
