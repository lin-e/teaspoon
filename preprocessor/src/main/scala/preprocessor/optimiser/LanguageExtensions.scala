package preprocessor.optimiser

import preprocessor.Common
import preprocessor.parser.ASTNodes._

object LanguageExtensions extends PipelineStage {
  override def process(m: Module)(implicit state: IRState): Module = {
    val processed = super.process(m)
    val imports = state.getImports.toList.map { i => ImportSpecifier(None, Identifier(i)) }
    val dec = ImportFrom(NamedImportsClause(imports), Literal("\"./combinator\""))
    Module(dec :: processed.body)
  }

  override def processStat(s: Statement)(implicit state: IRState): Statement = s match {
    case LazyVariableStatement(lhs, annotation, rhs) =>
      state.addImport("lazy")
      val le = MemberCall(Identifier("lazy"), Arguments(None, List(ArrowFunctionParams(CallSignature(None, None, None), processE(rhs)))))
      val vd = VariableDeclaration(processBPatt(lhs), annotation.map(processT), Some(le))
      VariableStatement("const", List(vd))
    case InlineVariableStatement(lhs, annotation, rhs) =>
      val vd = VariableDeclaration(processBPatt(lhs), annotation.map(processT), Some(processE(rhs)))
      VariableStatement("const", List(vd))
    case VariableStatement("val", vars) => VariableStatement("const", vars.map(process))
    case _ => super.processStat(s)
  }

  override def processE(e: Expression)(implicit state: IRState): Expression = e match {
    case BinaryOpExpr(e1, e2, op) => Common.customOperators.get(op) match {
      case Some(co) =>
        state.addImport(co.f)
        MemberCall(Identifier(co.f), Arguments(None, List(processE(e1), processE(e2))))
      case None => super.processE(e)
    }
    case InfixFuncExpr(e1, e2, f) => MemberCall(process(f), Arguments(None, List(processE(e1), processE(e2))))
    case _ => super.processE(e)
  }
}
