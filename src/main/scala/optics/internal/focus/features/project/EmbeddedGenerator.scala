package optics.internal.focus.features.project

import optics.internal.focus.FocusBase

private[focus] trait EmbeddedGenerator {
  this: FocusBase => 

  import macroContext.reflect._

  def generateEmbedded(opticExpr: Term): Term = {
    opticExpr.tpe.asType match {
      case tpe @ '[EmbeddedOptic[t, u, v]] =>
        '{ (${ opticExpr.asExprOf[tpe.Underlying]}).optic }.asTerm
    }
  }
}
