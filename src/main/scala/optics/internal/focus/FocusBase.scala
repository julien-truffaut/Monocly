package optics.internal.focus
import scala.quoted.Quotes

private[focus] trait FocusBase {
  val macroContext: Quotes 

  given Quotes = macroContext

  type Term = macroContext.reflect.Term
  type TypeRepr = macroContext.reflect.TypeRepr

  enum FocusAction {
    case FieldSelect(name: String, fromType: TypeRepr, fromTypeArgs: List[TypeRepr], toType: TypeRepr)
    case OptionSome(toType: TypeRepr)
    case EmbeddedOptic(toType: TypeRepr, opticType: TypeRepr, opticExpr: Term)

    override def toString(): String = this match {
      case FieldSelect(name, fromType, fromTypeArgs, toType) => s"FieldSelect($name, ${fromType.show}, ${fromTypeArgs.map(_.show).mkString("[", ",", "]")}, ${toType.show})"
      case OptionSome(toType) => s"OptionSome(${toType.show})"
      // The embedded expression is typically enormous...
      case EmbeddedOptic(toType, opticType, _) => s"EmbeddedOptic(${toType.show}, ${opticType.show}, ...)"
    }
  }

  enum FocusError {
    case NotACaseClass(className: String)
    case NotAConcreteClass(className: String)
    case DidNotDirectlyAccessArgument(argName: String)
    case NotASimpleLambdaFunction
    case UnexpectedCodeStructure(code: String)
    case CouldntFindFieldType(fromType: String, fieldName: String)
    case ComposeMismatch(type1: String, type2: String)

    def asResult: FocusResult[Nothing] = Left(this)
  }

  trait FocusParser {
    def unapply(term: Term): Option[FocusResult[(Term, FocusAction)]]
  }

  type FocusResult[+A] = Either[FocusError, A]
  type ParseResult = FocusResult[List[FocusAction]]
}
