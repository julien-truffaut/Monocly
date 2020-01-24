package optics

package object poly {

  type NonEmptyPTraversal[-S, +T, +A, -B] = EPTraversal[Nothing, S, T, A, B]
  type PTraversal[-S, +T, +A, -B]         = EPTraversal[Any, S, T, A, B]
  type POptional[-S, +T, +A, -B]          = EPOptional[Any, S, T, A, B]
  type PPrism[-S, +T, +A, -B]             = EPPrism[Any, S, T, A, B]

  type PLens[-S, +T, +A, -B] = EPOptional[Nothing, S, T, A, B]
  type PIso[-S, +T, +A, -B]  = EPPrism[Nothing, S, T, A, B]

}