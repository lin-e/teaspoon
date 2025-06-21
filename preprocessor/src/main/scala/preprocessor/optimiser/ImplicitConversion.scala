package preprocessor.optimiser

import preprocessor.Common._
import preprocessor.parser.ASTNodes._
import Utils._

object ImplicitConversion extends PipelineStage {
  override def processE(e: Expression)(implicit state: IRState): Expression = e match {
    case BinaryOpExpr(e1, e2, op) if customOperators.contains(op) =>
      // side that requires a Parser<T>
      val lhs = Set("<&>", "<?>", "$>")
      val rhs = Set("<$>", "<$")
      val both = customOperators.keySet -- (lhs ++ rhs)

      def convert(operand: Expression, set: Set[String]): Expression = if (set.contains(op)) {
        operand match {
          case Conversion(mc) => mc
          case _ => operand
        }
      } else {
        operand
      }

      BinaryOpExpr(processE(convert(e1, both ++ lhs)), processE(convert(e2, both ++ rhs)), op)
    case mc@MemberCall(id@Identifier(name), Arguments(t, args)) => reservedFunctions.get(name) match {
      case Some(ReservedFunction(parserArgs, _)) =>
        def convert(operand: Expression): Expression = operand match {
          case Conversion(mc) => mc
          case _ => super.processE(operand)
        }

        val newArgs = args
          .map(processE)
          .zipWithIndex
          .map { case (e, i) => (if (parserArgs.contains(i)) convert _ else identity[Expression] _)(e) }

        MemberCall(id, Arguments(t, newArgs))
      case None => super.processE(mc)
    }
    case _ => super.processE(e)
  }

  private object Conversion {
    def unapply(e: Expression): Option[MemberCall] = e match {
      case Literal(v) if charLiteral(v) => Some(MemberCall(Identifier("chr"), Arguments(None, List(e))))
      case Literal(v) if stringLiteral(v) => Some(MemberCall(Identifier("str"), Arguments(None, List(e))))
      case Literal(v) if regexLiteral(v) => Some(MemberCall(Identifier("re"), Arguments(None, List(e))))
      case _ => None
    }
  }
}
