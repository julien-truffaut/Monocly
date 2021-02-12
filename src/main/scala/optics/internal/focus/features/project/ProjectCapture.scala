package optics.internal.focus.features.project

import optics.internal.focus.{FocusBase, ErrorHandling}
import optics.internal.Function
import scala.language.experimental.namedTypeArguments
import scala.quoted.{Type, Expr, Quotes, quotes}
import optics.internal.focus.FocusImpl
import optics.poly.{Traversal, Lens, POptional, Optional}
import optics.poly.Focus.embed

//TODO: Add constraint once inheritance is back in
case class EmbeddedOptic[From, To, O](optic: O)

// Capture is meant to be used in the first `phase` of the Focus macro i.e. during the argument phase
class ProjectCaptureImpl(val macroContext: Quotes) extends FocusBase with ErrorHandling {

  import macroContext.reflect._

  // Transform an expression, changing `Ident(sym)` into the `to` term
  final class Rewrite(sym: String, to: Term) {
    def apply(expr: Term): Term =
      expr match {
        case Block(Nil, expr) => Block(Nil, apply(expr))
        case Typed(expr, tpt) => Typed(apply(expr), tpt)
        // TODO: Typed select thingy
        case Select(expr, name) => Select.unique(apply(expr), name)
        case Apply(app, List(expr)) => Apply(app, List(apply(expr)))
        case Ident(sym) if this.sym == sym => to
      }
  }

  object Cases {
    def unapply(expr: Term): Option[(Ident, List[CaseDef])] =
      expr match {
        case Block(List(DefDef(_, _, List(List(ValDef(valSym, _, _))), _, Some(Match(ident @ Ident(matchSym), cases)))), _)
            if matchSym == valSym => Some((ident, cases))
        case Block(List(), inner) => unapply(inner)
        case Inlined(_, _, inner) => unapply(inner)
        case _ => None
      }
  }

  object Case {
    def unapply(caseDef: CaseDef): Option[(Term, List[Tree], TypeTree, Term)] =
      caseDef match {
        case CaseDef(Typed(Unapply(Select(ident, _), _, bindings), tpt), _, expr) =>
          Some((ident, bindings, tpt, expr))
        case _ => None
      }
  }

  object IndexedFieldSym {
    def unapply(tuple: (Tree, Int)): Option[(Int, String)] = 
      tuple match {
        case (Bind(sym, _), idx) => Some((idx, sym))
        case _ => None
      }
  }

  final type CaseOptic = (TypeTree, Expr[Any])

  // CPS stands for Continuation-Passing-Style; It's /much/ easier to write this recursively
  // and leave the HotSpot to clean up our mess.
  def reifyCPSCase[T : Type](cases: List[CaseOptic], term: Term): Term  = {
    val expr = term.tpe.asType match {
      case '[u => (T => v)] =>
        val tailIfs: List[Expr[(T, () => v) => v]] = cases.map {
          case (tpt, opticExpr) =>
            (tpt.tpe.asType, opticExpr.asTerm.tpe.asType) match {
              case ('[t], '[uu]) =>
                '{
                val funcSelect = ${ term.asExprOf[u => (T => v)]}

                // TODO: Add this in properly once we have variance again...
                val optic = ${ opticExpr.asExprOf[uu] }
                val select = funcSelect(optic.asInstanceOf[u])
                (t: T, thunk: () => v) => if(t.isInstanceOf[t]) select(t) else thunk()
                }
            }
        }
        tailIfs.foldLeft(('{(t: T) => ???} : Expr[T => v])) {
          (expr, caseExpr) => '{ (t: T) => $caseExpr(t, () => $expr(t)) }
        }
    }
    expr.asTerm
  }

  // Expand recursively to use the `Focus` macro — this allows us to reify the optic /before/ passing it into the DSL.
  def reify[From : Type, To : Type](self: Expr[From], lambda: Expr[From => To]): Expr[Any] = {

    // TODO: This /needs/ to be invariant
    // TODO: If the Body is Any, Matchable or Nothing, the inference has probably failed — shortcircuit the expansion
    val Cases(ident, caseBranches) = lambda.asTerm

    def reifyCases(cases: List[CaseDef]): FocusResult[List[CaseOptic]] = {
      cases match {
        case Nil => Right(Nil)
        case (caseExpr @ Case(ident, bindings, caseType, expr)) :: cases =>
          val boundVars = bindings.zipWithIndex.collect {
            case IndexedFieldSym(idx, sym) => (idx, sym)
          }

          if (boundVars.length != 1)
            Left(FocusError.UnexpectedCodeStructure(caseExpr.show))
          else {

            val (fieldIdx, boundSym) = boundVars.head
            val fieldName = for {
              classSym <- caseType.tpe.classSymbol
              // TODO: This /seems/ to be non-deterministic every so often...?
              // Verify this is invariant
              memberField <- classSym.caseFields.lift(fieldIdx)
            } yield memberField.name

            // TODO: Prove whether this can actually fail
            val optic = fieldName match {
              case Some(name) =>
                caseType.tpe.asType match {
                  case '[t] =>
                    val lambdaExpr =
                      '{ (x: t) => ${ new Rewrite(boundSym, Select.unique('{x}.asTerm, name))(expr).asExprOf[To] } }
                    FocusImpl[t, To](lambdaExpr)
                }
              case _ => throw new RuntimeException(s"Expansion failed catastrophically, ${caseExpr.show}")
            }
            reifyCases(cases).map(caseType -> optic :: _)
          }
        case expr :: _ =>
          Left(FocusError.UnexpectedCodeStructure(expr.show))
      }
    }

    // We /must/ use the fully-qualified names, otherwise `.tpe` doesn't work properly
    val get = '{ (optic: Lens[From, To]) => optic.get }
    val getOption = '{ (optic: POptional[From, From, To, To]) => optic.getOption }

    // Flip the setters to make it easier to transform
    val replace = '{ (optic: POptional[From, From, To, To]) => Function.flip(optic.replace) }

    // Everything to turn our `caseOptic` into a fully-fledged optic
    val optionalReify = ('{ Optional.apply[From, To] }.asTerm, getOption.asTerm, replace.asTerm)
    val lensReify = ('{ Lens.apply[From, To]}.asTerm, get.asTerm, replace.asTerm)

    // Each of the branches can theoretically be a different optic
    val result = reifyCases(caseBranches).map { caseOptics =>

      // TODO: This needs fixing once we have variance!
      // Find the least powerful optic
      val opticReify = caseOptics.map(_._2.asTerm.tpe.asType).foldLeft(lensReify) {
        case (pair, '[Optional[u, v]])  => optionalReify
        case (pair, '[Lens[u, v]]) if !Set(optionalReify).contains(pair) => lensReify
        case (pair, _) => pair
      }

      val (cons, first, second) = opticReify

      val setterExpr = reifyCPSCase[From](caseOptics, second)
      val getterExpr = reifyCPSCase[From](caseOptics, first)

      cons.tpe.asType match {
        case tpe @ '[(t, u => v => w) => o] =>
          '{
             val makeOptic = ${ cons.asExprOf[tpe.Underlying] }
             val getter = ${ getterExpr.asExprOf[t] }
             val setter = ${ setterExpr.asExprOf[v => u => w] }
             makeOptic(getter, Function.flip(setter))
           }
      }
    }

    result match {
      case Right(expr) =>
        expr.asTerm.tpe.asType match {
          case '[Lens[From, To]] =>
            '{ $self.embed[From, To, Lens[From, To]](EmbeddedOptic(${expr.asTerm.asExprOf[Lens[From, To]]})) }
          case '[Optional[From, To]] =>
            '{ $self.embed[From, To, Optional[From, To]](EmbeddedOptic(${expr.asTerm.asExprOf[Optional[From, To]]})) }
          case '[Traversal[From, To]] =>
            '{ $self.embed[From, To, Traversal[From, To]](EmbeddedOptic(${expr.asTerm.asExprOf[Traversal[From, To]]})) }
        }
      case Left(error) =>
        report.error(errorMessage(error))
        '{ ??? }
    }
  }
}

object ProjectCaptureImpl {
  def apply[From : Type, To: Type](value: Expr[From], lambda: Expr[From => To])(using Quotes): Expr[Any] =
    new ProjectCaptureImpl(quotes).reify(value, lambda)
}
