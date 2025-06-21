package preprocessor.optimiser

import preprocessor.parser.ASTNodes._

object IRNodes {

  // extend Expression so we can use existing AST
  trait IR extends Expression {
    override def print(): String = f"[IR <$this>]" // Error case
  }

  case class IRLiteral(l: Literal) extends IR

  case class Atom(id: Int) extends IR

  case class Id(name: String) extends IR with BindingPattern with PropertyDefinition

  sealed trait Combinator extends IR {
    val l: IR
    val r: IR
  }

  case class Ap(override val l: IR, override val r: IR) extends Combinator

  case class Pa(override val l: IR, override val r: IR) extends Combinator

  case class ApL(override val l: IR, override val r: IR) extends Combinator

  case class ApR(override val l: IR, override val r: IR) extends Combinator

  case class Choice(override val l: IR, override val r: IR) extends Combinator

  case class Mult(override val l: IR, override val r: IR) extends Combinator

  case class MultL(override val l: IR, override val r: IR) extends Combinator

  case class MultR(override val l: IR, override val r: IR) extends Combinator

  case class Fmap(override val l: IR, override val r: IR) extends Combinator

  case class Pamf(override val l: IR, override val r: IR) extends Combinator

  case class ConstFmapL(override val l: IR, override val r: IR) extends Combinator

  case class ConstFmapR(override val l: IR, override val r: IR) extends Combinator

  case class LiftCons(override val l: IR, override val r: IR) extends Combinator

  case class Label(override val l: IR, override val r: IR) extends Combinator

  sealed trait Func extends IR {
    val typeArgs: Option[TypeArguments] = None
    val args: List[IR] = Nil
  }

  case class Pure(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  case class Satisfy(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  case class Chr(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  case class Str(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  case class Many1(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  case class Many(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  case class Attempt(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  case class Token(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  case class CT(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  case class ChainL1(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  case class ChainR1(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  case class Lift2(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  case class Lift3(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  case class Lift4(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  case class Lift5(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  case class Lift6(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  case class Lift7(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  case class Lift8(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  case class Lift9(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  case class Postfix(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  case class Empty(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  case class Between(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  case class Re(override val typeArgs: Option[TypeArguments], override val args: List[IR]) extends Func

  sealed trait Generated extends IR

  // special cases; generated from optimisers
  sealed trait GeneratedFunc extends Generated

  case class Curry(f: IR) extends GeneratedFunc

  case class Flip(f: IR) extends GeneratedFunc

  case class Const(c: IR) extends GeneratedFunc

  case class MapFirstArg(g: IR, f: IR) extends GeneratedFunc

  case class Uncurry(f: IR) extends GeneratedFunc

  case class UnpairFirstArg(f: IR) extends GeneratedFunc

  case class PaTransform(f: IR) extends GeneratedFunc

  case class ArgsToTuple(f: IR) extends GeneratedFunc

  case class LiftStringConcat(p: IR, q: IR) extends GeneratedFunc

  sealed trait Special extends Generated

  case class Explicit(e: Expression) extends Special

  case object ConstFunction extends Special

  case object ApFromMult extends Special

  case object PaFromMult extends Special

  case object StringEpsilon extends Special

  case object Cons extends Special

  case class LeadingChar(s: String) extends Special

//  case class CurryThenFlip(override val args: List[IR]) extends GeneratedFunc
//
//  case class MapFirstArg(override val args: List[IR]) extends GeneratedFunc
//
//  case class ConstMapFirstArg(override val args: List[IR]) extends GeneratedFunc
//
//  case class Uncurry(override val args: List[IR]) extends GeneratedFunc
}
