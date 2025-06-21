package preprocessor.optimiser

import preprocessor.Common._
import preprocessor.optimiser.IRNodes._
import preprocessor.parser.ASTNodes._

object ToIR extends PipelineStage {

  override def processE(e: Expression)(implicit state: IRState): Expression = expressionToIR(e)

  override def processPD(pd: PropertyDefinition)(implicit state: IRState): PropertyDefinition = pd match {
    case i: Identifier => idToIR(i)
    case _ => super.processPD(pd)
  }

  override def processBPatt(bp: BindingPattern)(implicit state: IRState): BindingPattern = bp match {
    case i: Identifier => idToIR(i)
    case _ => super.processBPatt(bp)
  }

  private object ReservedCall {
    def unapply(e: Expression)(implicit state: IRState): Option[IR] = e match {
      case MemberCall(Identifier(name), Arguments(typeArgs, args)) => reservedFunctions.get(name) match {
        case Some(ReservedFunction(_, ir)) => Some(ir(typeArgs, args.map(expressionToIR)))
        case _ => None
      }
      case _ => None
    }
  }

  private object BinaryCombinator {
    def unapply(e: Expression)(implicit state: IRState): Option[IR] = e match {
      case BinaryOpExpr(e1, e2, op) => customOperators.get(op) match {
        case Some(CustomOperator(_, _, ir)) => Some(ir(expressionToIR(e1), expressionToIR(e2)))
        case _ => None
      }
      case _ => None
    }
  }

  private def expressionToIR(e: Expression)(implicit state: IRState): IR = e match {
    case l: Literal => IRLiteral(super.process(l))
    case i: Identifier => idToIR(i)
    case ReservedCall(ir) => ir
    case BinaryCombinator(ir) => ir
    case _ => state.getAtom(super.processE(e))
  }

  private def idToIR(i: Identifier)(implicit state: IRState): Id = Id(i.name)
}
