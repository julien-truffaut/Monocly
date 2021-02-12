package optics.internal.focus.features.project

import optics.internal.focus.FocusBase

private[focus] trait EmbedParser {
  this: FocusBase =>

  import macroContext.reflect._

  object EmbeddedOptic extends FocusParser {

    def unapply(term: Term): Option[FocusResult[(Term, FocusAction)]] = term match {
      case Apply(Apply(TypeApply(Ident("embed"), List(_, toTypeArg, opticTypeArg)), List(remainingCode)), List(opticExpr))  =>
        val toType = toTypeArg.tpe
        val opticType = opticTypeArg.tpe
        val action = FocusAction.EmbeddedOptic(toType, opticType, opticExpr)
        Some(Right(remainingCode, action))
      case _ => None
    }
  }
}
