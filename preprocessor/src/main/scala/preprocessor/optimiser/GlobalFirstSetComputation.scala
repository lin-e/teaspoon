package preprocessor.optimiser

import preprocessor.parser.ASTNodes._
import IRNodes._

object GlobalFirstSetComputation extends PipelineStage {

  override def process(m: Module)(implicit state: IRState): Module = {
    val processed = super.process(m)

    // Compute global first set after entire AST is traversed
    state.computedGFS()

    processed
  }

  override def processStat(s: Statement)(implicit state: IRState): Statement = {
    s match {
      case LazyVariableStatement(lhs: Id, _, rhs: IR) => state.addProduction(lhs, rhs)
      case VariableStatement(_, vars) => vars.foreach {
        case VariableDeclaration(lhs: Id, _, Some(ir: IR)) => state.addProduction(lhs, ir)
        case _ => ()
      }
      case _ => ()
    }

    super.processStat(s)
  }
}
