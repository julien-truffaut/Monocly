package optics.poly

import optics.poly.Traversal
import optics.internal.focus.{FocusImpl, AppliedFocusImpl}
import optics.internal.focus.FocusImpl
import optics.internal.Applicative
import optics.internal.focus.features.project.ProjectCaptureImpl
import optics.internal.focus.features.project.EmbeddedOptic

object Focus {

  class AppliedLens[S, T, A, B](from: S, underlying: PLens[S, T, A, B]) {
    def get: A = underlying.get(from)
    def replace(to: B): T = underlying.replace(to)(from)
    def modify(f: A => B): T = underlying.modify(f)(from)
    export underlying.{some, adapt, andThen, asTraversal, asOptional}
  }

  class AppliedPrism[S, T, A, B](from: S, underlying: PPrism[S, T, A, B]) {
    def getOrModify: Either[T, A] = underlying.getOrModify(from)
    def getOption: Option[A] = underlying.getOption(from)
    def replace(to: B): T = underlying.replace(to)(from)
    export underlying.{reverseGet, andThen, asTraversal, asOptional}
  }

  class AppliedIso[S, T, A, B](from: S, underlying: PIso[S, T, A, B]) {
    def get: A = underlying.get(from)
    export underlying.{reverseGet, andThen, asTraversal, asOptional, asLens, asPrism}
  }

  class AppliedOptional[S, T, A, B](from: S, underlying: POptional[S, T, A, B]) {
    def getOrModify: Either[T, A] = underlying.getOrModify(from)
    def replace(to: B): T = underlying.replace(to)(from)
    def modifyF[F[_] : Applicative](f: A => F[B]): F[T] = underlying.modifyF(f)(from)
    def modify(f: A => B): T = underlying.modify(f)(from)
    def getOption: Option[A] = underlying.getOption(from)
    export underlying.{some, adapt, andThen, asTraversal}
  }

  extension [From, To] (from: From) 
    transparent inline def focus(inline lambda: (From => To)): Any = 
      ${AppliedFocusImpl[From, To]('from, 'lambda)}

  extension [A] (opt: Option[A])
    def some: A = scala.sys.error("Extension method 'some' should only be used within the optics.poly.Focus macro.")

  extension [A, B, O](a: A)
    def embed(o: EmbeddedOptic[A, B, O]): B = scala.sys.error("Extension method 'embed' should only be used within the optics.poly.Focus macro.")

  // Phase-1
  extension [A, B] (a: A)
    transparent inline def project(inline get: A => B) = ${ ProjectCaptureImpl('a, 'get) }

  class MkFocus[From] {
    transparent inline def apply[To](inline lambda: (From => To)): Any = 
      ${ FocusImpl('lambda) }
  }

  def apply[A]: MkFocus[A] =  new MkFocus[A]
}
