package optics.poly.focus

import optics.poly.Focus
import optics.poly.Focus._

final class FocusProjectTest extends munit.FunSuite {

  test("Projection for a coproduct") {
    sealed trait Pet
    case class Dog(name: String) extends Pet
    case class Cat(name: String) extends Pet

    val name = Focus[Pet] {
      _.project {
        case Dog(name) => name
        case Cat(name) => name
      }
    }

    val fido = Dog("Fido")
    val meow = Cat("Meow")

    assertEquals(name.get(fido), fido.name)
    assertEquals(name.get(meow), meow.name)
  }

  test("Projection for nested Focus") {

    case class Hand(fingers: Int)

    sealed trait Pirate

    case class OneHanded(leftHand: Option[Hand]) extends Pirate

    // TODO: For the moment this is broken because our macro assumes inheritance
    case class TwoHanded(leftHand: Option[Hand]) extends Pirate

    val captainHook = OneHanded(None)
    val longJohn = OneHanded(Some(Hand(5)))

    val fingers = Focus[Pirate] {
      _.project {
        case OneHanded(lh) => lh.some
        case TwoHanded(lh) => lh.some
      }.fingers
    }

    assertEquals(fingers.getOption(captainHook), None)
    assertEquals(fingers.getOption(longJohn), Some(5))
  }
}
