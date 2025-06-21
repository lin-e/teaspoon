package preprocessor.parser

import preprocessor.Common._

object ASTNodes {

  sealed trait AST {
    def print(): String = ???
  }

  trait Expression extends AST with ConciseBody
  trait PropertyDefinition extends AST
  sealed trait ModuleItem extends AST
  sealed trait ImportDeclaration extends ModuleItem
  sealed trait ImportClause extends AST
  sealed trait Statement extends StatementListItem
  sealed trait StatementListItem extends ModuleItem
  sealed trait ExportDeclaration extends ModuleItem
  trait BindingPattern extends AST // this will include BindingIdentifier, just for the sake of simplicity
  sealed trait SwitchClause extends AST
  sealed trait ArrowFunction extends Expression
  sealed trait ConciseBody extends AST
  sealed trait Type extends AST
  sealed trait BindingProperty extends AST
  sealed trait BindingElement extends AST
  sealed trait Declaration extends StatementListItem
  sealed trait FunctionDeclaration extends Declaration
  sealed trait Signature extends AST
  sealed trait ParameterListItem extends AST

  // trait for all nodes that don't need any further traversal
  sealed trait IRLeaf

  case class Literal(value: String) extends Expression with Type {
    override def print(): String = value
  }

  case class Identifier(name: String) extends Expression with PropertyDefinition with BindingPattern {
    override def print(): String = name
  }

  case class ArrayLiteral(elems: List[Expression]) extends Expression {
    override def print(): String = f"[${elems.print()}]"
  }

  case class ObjectLiteral(defs: List[PropertyDefinition]) extends Expression {
    override def print(): String = f"{${defs.print(", ")}}"
  }
  case class Spread(id: Identifier) extends Expression {
    override def print(): String = f"...${id.print()}"
  }

  case class Module(body: List[ModuleItem]) extends AST {
    override def print(): String = body.print("", "; ", ";")
  }

  // both string literals
  case class ImportModuleSpecifier(specifier: Literal) extends ImportDeclaration {
    override def print(): String = f"import ${specifier.print()}"
  }

  case class ImportFrom(clause: ImportClause, specifier: Literal) extends ImportDeclaration {
    override def print(): String = f"import ${clause.print()} from ${specifier.print()}"
  }

  case class ImportRequire(binding: Identifier, specifier: Literal) extends ImportDeclaration {
    override def print(): String = f"import ${binding.print()} = require(${specifier.print()})"
  }

  case class ImportBindingClause(binding: Identifier) extends ImportClause {
    override def print(): String = binding.print()
  }

  case class NameSpaceImportClause(binding: Identifier) extends ImportClause {
    override def print(): String = f"* as ${binding.print()}"
  }

  case class NamedImportsClause(specs: List[ImportSpecifier]) extends ImportClause {
    override def print(): String = f"{${specs.print(", ")}}"
  }
  case class ImportSpecifier(name: Option[Identifier], binding: Identifier) extends AST {
    override def print(): String = name match {
      case Some(value) => f"${value.print()} as ${binding.print()}"
      case None => binding.print()
    }
  }

  // ns is either NameSpaceImportClause or NamedImportsClause
  case class BindingFollowedClause(b: ImportBindingClause, ns: ImportClause) extends ImportClause {
    override def print(): String = f"${b.print()}, ${ns.print()}"
  }

  case class ExportStar(specifier: Literal) extends ExportDeclaration {
    override def print(): String = f"export * from ${specifier.print()}"
  }

  case class ExportWithClause(ec: ExportClause, from: Option[Literal]) extends ExportDeclaration {
    override def print(): String = f"export ${ec.print()} ${from.print()}"
  }

  case class ExportVar(s: VariableStatement) extends ExportDeclaration {
    override def print(): String = f"export ${s.print()}"
  }

  // function declaration
  case class ExportFun(f: FunctionDeclaration) extends ExportDeclaration {
    override def print(): String = f"export ${f.print()}"
  }
  // todo hoistable and class

  case class ExportClause(specs: List[ExportSpecifier]) extends AST {
    override def print(): String = f"{${specs.print(", ")}}"
  }

  case class ExportSpecifier(id: Identifier, as: Option[Identifier]) extends AST {
    override def print(): String = as match {
      case Some(value) => f"${id.print()} as ${value.print()}"
      case None => id.print()
    }
  }

  case class StatementList(stats: List[StatementListItem]) extends AST {
    override def print(): String = stats.print("", "; ", ";")
  }
  case class ExpressionSequence(es: List[Expression]) extends Statement {
    override def print(): String = es.print(", ")
  }

  case class PostfixExpr(e: Expression, op: String) extends Expression {
    override def print(): String = e.print() + op
  }
  case class KeywordPrefixExpr(e: Expression, op: String) extends Expression { // for void / delete / typeof
    override def print(): String = f"$op ${e.print()}"
  }
  case class PrefixExpr(e: Expression, op: String) extends Expression {
    override def print(): String = op + e.print()
  }
  case class TypeCastExpr(t: Type, e: Expression) extends Expression {
    override def print(): String = f"(<${t.print()}> ${e.print()})"
  }
  case class BinaryOpExpr(e1: Expression, e2: Expression, op: String) extends Expression {
    override def print(): String = f"(${e1.print()} $op ${e2.print()})"
  }

  case class InfixFuncExpr(e1: Expression, e2: Expression, f: Identifier) extends Expression

  case class TernaryExpr(p: Expression, e1: Expression, e2: Expression) extends Expression {
    override def print(): String = f"(${p.print()} ? ${e1.print()} : ${e2.print()})"
  }
  case class MemberDotExpr(e: Expression, i: Identifier) extends Expression {
    override def print(): String = f"${e.print()}.${i.print()}"
  }
  case class MemberIndexExpr(e: Expression, es: ExpressionSequence) extends Expression {
    override def print(): String = f"${e.print()}[${es.print()}]"
  }
  case class ParenthesisedExpr(es: ExpressionSequence) extends Expression {
    override def print(): String = f"(${es.print()})"
  }

//  case class NewExpr(e: Expression, args: Arguments) extends Expression {
//    override def print(): String = f"new ${e.print()}${args.print()}"
//  }
//  case class NewExprNoArgs(e: Expression) extends Expression {
//    override def print(): String = f"new ${e.print()}"
//  }

  case class NewMemberExpr(expr: Expression) extends Expression {
    override def print(): String = f"new ${expr.print()}"
  }

  case object Elision extends Expression {
    override def print(): String = ","
  }
  case object This extends Expression {
    override def print(): String = "this"
  }
  case object Super extends Expression {
    override def print(): String = "super"
  }
  case object Yield extends Expression {
    override def print(): String = "yield"
  }

  // check this, name isn't technically correct
  case class PropertyExpressionAssignment(name: Expression, e: Expression) extends PropertyDefinition {
    override def print(): String = f"${name.print()}: ${e.print()}"
  }

  case class CoverInitialisedName(name: Expression, e: Expression) extends PropertyDefinition {
    override def print(): String = f"${name.print()} = ${e.print()}"
  }

  //  case class ComputedPropertyExpressionAssignment(e1: Expression, e2: Expression) extends PropertyDefinition {
  //    override def print(): String = f"[${e1.print()}]: ${e2.print()}"
  //  }

  case class MethodProperty(name: Expression, call: CallSignature, body: StatementList) extends PropertyDefinition {
    override def print(): String = f"${name.print()} ${call.print()} { ${body.print()} }"
  }

  // not really an expression
  case class ComputedPropertyName(e: Expression) extends Expression {
    override def print(): String = f"[${e.print()}]"
  }

  // Identifier done above
  // TODO:
  // getter
  // setter
  // generator

  case class MemberCall(member: Expression, args: Arguments) extends Expression {
    override def print(): String = f"${member.print()}${args.print()}"
  }
  case class SuperCall(args: Arguments) extends Expression {
    override def print(): String = f"super${args.print()}"
  }
  case class NestedCall(call: Expression, args: Arguments) extends Expression {
    override def print(): String = f"${call.print()}${args.print()}"
  }
  case class IndexedCall(call: Expression, es: ExpressionSequence) extends Expression {
    override def print(): String = f"${call.print()}[${es.print()}]"
  }
  case class DotCall(call: Expression, i: Identifier) extends Expression {
    override def print(): String = f"${call.print()}.${i.print()}"
  }
  // TODO: template

  case class IndexedMember(member: Expression, e: Expression) extends Expression {
    override def print(): String = f"${member.print()}[${e.print()}]"
  }
  case class DotMember(member: Expression, i: Identifier) extends Expression {
    override def print(): String = f"${member.print()}.${i.print()}"
  }
  case object MetaProperty extends Expression {
    override def print(): String = "new.target"
  }
  case class SuperPropertyDot(i: Identifier) extends Expression {
    override def print(): String = f"super.${i.print()}"
  }
  case class SuperPropertyIndexed(e: Expression) extends Expression {
    override def print(): String = f"super[${e.print()}]"
  }

  case object EmptyStatement extends Statement {
    override def print(): String = "" // make all statements join with ; at end
  }
  case object DebuggerStatement extends Statement {
    override def print(): String = "debugger"
  }

  case class VariableStatement(modifier: String, vars: List[VariableDeclaration]) extends Statement {
    override def print(): String = f"$modifier ${vars.print(", ")}"
  }

//  case class LazyVariableStatement(lhs: BindingPattern, annotation: Option[Type], rhs: Expression) extends Statement {
//    override def print(): String = annotation match {
//      case Some(value) => f"let ${lhs.print()}: ${value.print()} = lazy(() => ${rhs.print()})"
//      case None => f"let ${lhs.print()} = lazy(() => ${rhs.print()})"
//    }
//  }
//
//  case class InlineVariableStatement(lhs: BindingPattern, annotation: Option[Type], rhs: Expression) extends Statement {
//    override def print(): String = annotation match {
//      case Some(value) => f"const ${lhs.print()}: ${value.print()} = ${rhs.print()}"
//      case None => f"const ${lhs.print()} = ${rhs.print()}"
//    }
//  }
//
  case class LazyVariableStatement(lhs: BindingPattern, annotation: Option[Type], rhs: Expression) extends Statement

  case class InlineVariableStatement(lhs: BindingPattern, annotation: Option[Type], rhs: Expression) extends Statement

  case class InlineFunctionStatement(lhs: Identifier, params: List[Identifier], rhs: Expression) extends Statement

  case class VariableDeclaration(pattern: BindingPattern, annotation: Option[Type], expr: Option[Expression]) extends AST {
    override def print(): String = (annotation, expr) match {
      case (Some(t), Some(e)) => f"${pattern.print()}: ${t.print()} = ${e.print()}"
      case (Some(t), None) => f"${pattern.print()}: ${t.print()}"
      case (None, Some(e)) => f"${pattern.print()} = ${e.print()}"
      case (None, None) => pattern.print()
    }
  }

  case class BlockStatement(stats: StatementList) extends Statement {
    override def print(): String = f"{ ${stats.print()} }"
  }

  case class IfStatement(p: ExpressionSequence, t: Statement, f: Option[Statement]) extends Statement {
    override def print(): String = {
      val fPart = f match {
        case Some(value) =>
          f"""else
             |  ${value.print()}
             |""".stripMargin
        case None => ""
      }

      f"""if (${p.print()})
         |  ${t.print()}
         |""".stripMargin + fPart
    }
  }

  case class DoStatement(s: Statement, p: ExpressionSequence) extends Statement {
    override def print(): String =
      f"""do
         |  ${s.print()}
         |while (${p.print()});
         |""".stripMargin
  }

  case class WhileStatement(p: ExpressionSequence, s: Statement) extends Statement {
    override def print(): String =
      f"""while (${p.print()})
         |  ${s.print()}
         |""".stripMargin
  }

  case class ForStatement(e1: Option[ExpressionSequence], e2: Option[ExpressionSequence], e3: Option[ExpressionSequence], s: Statement) extends Statement {
    override def print(): String =
      f"""for (${e1.print()}; ${e2.print()}; ${e3.print()})
         |  ${s.print()}
         |""".stripMargin
  }

  case class ForVarStatement(modifier: String, vars: List[VariableDeclaration], e1: Option[ExpressionSequence], e2: Option[ExpressionSequence], s: Statement) extends Statement {
    override def print(): String =
      f"""for ($modifier ${vars.print(", ")}; ${e1.print()}; ${e2.print()})
         |  ${s.print()}
         |""".stripMargin
  }

  // io: in/of
  case class ForInStatement(e: Expression, io: String, es: ExpressionSequence, s: Statement) extends Statement {
    override def print(): String =
      f"""for (${e.print()} $io ${es.print()})
         |  ${s.print()}
         |""".stripMargin
  }

  case class ForVarInStatement(modifier: String, vd: VariableDeclaration, io: String, es: ExpressionSequence, s: Statement) extends Statement {
    override def print(): String =
      f"""for ($modifier ${vd.print()} $io ${es.print()})
         |  ${s.print()}
         |""".stripMargin
  }

  case class ContinueStatement(id: Option[Identifier]) extends Statement {
    override def print(): String = f"""continue ${id.print()}"""
  }

  case class BreakStatement(id: Option[Identifier]) extends Statement {
    override def print(): String = f"""break ${id.print()}"""
  }

  case class ReturnStatement(es: Option[ExpressionSequence]) extends Statement {
    override def print(): String = f"""return ${es.print()}"""
  }

  case class ThrowStatement(es: ExpressionSequence) extends Statement {
    override def print(): String = f"""throw ${es.print()}"""
  }

  case class WithStatement(es: ExpressionSequence, s: Statement) extends Statement {
    override def print(): String =
      f"""with (${es.print()})
         |  ${s.print()}
         |""".stripMargin
  }

  case class SwitchStatement(es: ExpressionSequence, clauses: List[SwitchClause]) extends Statement {
    override def print(): String =
      f"""switch (${es.print()}) {
         |  ${clauses.print("\n")}
         |}
         |""".stripMargin
  }

  case class CaseClause(es: ExpressionSequence, ss: StatementList) extends SwitchClause {
    override def print(): String =
      f"""case ${es.print()}:
         |  ${ss.print()}
         |""".stripMargin
  }

  case class DefaultClause(ss: StatementList) extends SwitchClause {
    override def print(): String =
      f"""default:
         |  ${ss.print()}
         |""".stripMargin
  }

  case class LabelledStatement(id: Identifier, s: Statement) extends Statement {
    override def print(): String = f"${id.print()}: ${s.print()}"
  }

  case class CatchProduction(pattern: BindingPattern, b: Statement) extends AST {
    override def print(): String = f"""catch (${pattern.print()}) ${b.print()}"""
  }
  case class FinallyProduction(b: Statement) extends AST {
    override def print(): String = f"""finally ${b.print()}"""
  }

  case class TryCatchStatement(b: Statement, c: CatchProduction, f: Option[FinallyProduction]) extends Statement {
    override def print(): String =
      f"""try ${b.print()}
         |${c.print()}
         |${f.print()}
         |""".stripMargin
  }

  case class TryFinallyStatement(b: Statement, f: FinallyProduction) extends Statement {
    override def print(): String =
      f"""try ${b.print()}
         |${f.print()}
         |""".stripMargin
  }

  case class BindingObject(props: List[BindingProperty]) extends BindingPattern {
    override def print(): String = f"{ ${props.print(", ")} }"
  }

  case class BindingArray(elems: List[BindingElement]) extends BindingPattern {
    override def print(): String = f"[${elems.print()}]"
  }

  case object BindingElision extends BindingElement {
    override def print(): String = ","
  }

  // id is just Identifier; this makes the IR easier
  case class SingleNameBinding(id: BindingPattern, init: Option[Expression]) extends BindingProperty with BindingElement {
    override def print(): String = {
      init match {
        case Some(value) => f"${id.print()} = ${value.print()}"
        case None => id.print()
      }
    }
  }

  case class BindingPatternElement(pattern: BindingPattern, init: Option[Expression]) extends BindingElement {
    override def print(): String = {
      init match {
        case Some(value) => f"${pattern.print()} = ${value.print()}"
        case None => pattern.print()
      }
    }
  }

  case class BindingPropertyName(property: Expression, element: BindingElement) extends BindingProperty {
    override def print(): String = f"${property.print()}: ${element.print()}"
  }

  case class BindingSpread(id: Identifier) extends BindingElement {
    override def print(): String = f"...${id.print()}"
  }

  case class NonEmptyFunctionDeclaration(id: Option[Identifier], sig: CallSignature, body: StatementList) extends FunctionDeclaration {
    override def print(): String =
      f"""function ${id.print()}${sig.print()} {
         |  ${body.print()}
         |}
         |""".stripMargin
  }

  case class FunctionExpression(id: Option[Identifier], sig: CallSignature, body: StatementList) extends Expression {
    override def print(): String =
      f"""function ${id.print()}${sig.print()} {
         |  ${body.print()}
         |}
         |""".stripMargin
  }

  case class EmptyFunctionDeclaration(id: Option[Identifier], sig: CallSignature) extends FunctionDeclaration {
    override def print(): String = f"function ${id.print()}${sig.print()};"
  }

  case class FunctionBody(body: StatementList) extends ConciseBody {
    override def print(): String = f"{ ${body.print()} }"
  }

  case class ArrowFunctionIdentifier(id: Identifier, body: ConciseBody) extends ArrowFunction {
    override def print(): String = f"(${id.print()} => ${body.print()})"
  }
  case class ArrowFunctionParams(sig: CallSignature, body: ConciseBody) extends ArrowFunction{
    override def print(): String = f"(${sig.print()} => ${body.print()})"
  }

  case object AnyType extends Type {
    override def print(): String = "any"
  }

  case object NumberType extends Type {
    override def print(): String = "number"
  }

  case object BooleanType extends Type {
    override def print(): String = "boolean"
  }

  case object StringType extends Type {
    override def print(): String = "string"
  }

  case object SymbolType extends Type {
    override def print(): String = "symbol"
  }

  case object VoidType extends Type {
    override def print(): String = "void"
  }

  case object ThisType extends Type {
    override def print(): String = "this"
  }

  case class UnionType(t1: Type, t2: Type) extends Type {
    override def print(): String = f"(${t1.print()} | ${t2.print()})"
  }

  case class IntersectionType(t1: Type, t2: Type) extends Type {
    override def print(): String = f"(${t1.print()} & ${t2.print()})"
  }

  case class ParenthesisedType(t: Type) extends Type {
    override def print(): String = f"(${t.print()})"
  }

  case class ArrayType(t: Type) extends Type {
    override def print(): String = f"${t.print()}[]"
  }

  case class TupleType(ts: List[Type]) extends Type {
    override def print(): String = f"[${ts.print(", ")}]"
  }

  case class TypeParameters(params: List[TypeParameter]) extends AST {
    override def print(): String = f"<${params.print(", ")}>"
  }

  case class TypeParameter(id: Identifier, constraint: Option[Type]) extends AST {
    override def print(): String = constraint match {
      case Some(value) => f"${id.print()} extends ${value.print()}"
      case None => id.print()
    }
  }

  case class TypeArguments(ts: List[Type]) extends AST {
    override def print(): String = f"<${ts.print(", ")}>"
  }

  case class TypeReference(refs: List[Expression], args: Option[TypeArguments]) extends Type {
    override def print(): String = f"${refs.print(".")} ${args.print()}"
  }

  case class TypeQuery(refs: List[Expression]) extends Type {
    override def print(): String = f"typeof ${refs.print(".")}"
  }


  case class RequiredIdentifierOrPattern(modifier: Option[String], bind: BindingPattern, annotation: Option[Type]) extends ParameterListItem {
    override def print(): String = {
      val (m, a) = modifierAnnotationToString(modifier, annotation)
      f"$m${bind.print()} $a"
    }
  }

  case class RequiredIdentifierWithString(id: Identifier, s: Literal) extends ParameterListItem {
    override def print(): String = f"${id.print()} : ${s.print()}"
  }

  case class OptionalIdentifierOrPattern(modifier: Option[String], bind: BindingPattern, annotation: Option[Type]) extends ParameterListItem {
    override def print(): String = {
      val (m, a) = modifierAnnotationToString(modifier, annotation)
      f"$m${bind.print()}? $a"
    }
  }

  case class OptionalIdentifierOrPatternInit(modifier: Option[String], bind: BindingPattern, annotation: Option[Type], e: Expression) extends ParameterListItem {
    override def print(): String = {
      val (m, a) = modifierAnnotationToString(modifier, annotation)
      f"$m${bind.print()}? $a = ${e.print()}"
    }
  }

  case class OptionalIdentifierWithString(id: Identifier, s: Literal) extends ParameterListItem {
    override def print(): String = f"${id.print()}? : ${s.print()}"
  }

  case class RestSpreadParameter(id: Identifier, annotation: Option[Type]) extends ParameterListItem {
    override def print(): String = annotation match {
      case Some(value) => f"...${id.print()}: ${value.print()}"
      case None => f"...${id.print()}"
    }
  }

  case class ParameterList(contents: List[ParameterListItem]) extends AST {
    override def print(): String = contents.print(", ")
  }

  // name is identifier, stringliteral, or numericliteral
  case class PropertySignature(name: Expression, q: String, annotation: Option[Type]) extends Signature {
    override def print(): String = annotation match {
      case Some(value) => f"${name.print()}$q : ${value.print()}"
      case None => f"${name.print()}$q"
    }
  }

  case class CallSignature(tParams: Option[TypeParameters], params: Option[ParameterList], annotation: Option[Type]) extends Signature {
    override def print(): String = annotation match {
      case Some(value) => f"${tParams.print()}(${params.print()}): ${value.print()}"
      case None => f"${tParams.print()}(${params.print()})"
    }
  }

  case class ConstructSignature(tParams: Option[TypeParameters], params: Option[ParameterList], annotation: Option[Type]) extends Signature {
    override def print(): String = annotation match {
      case Some(value) => f"new ${tParams.print()}(${params.print()}): ${value.print()}"
      case None => f"new ${tParams.print()}(${params.print()})"
    }
  }

  case class IndexSignature(id: Identifier, t: String, annotation: Type) extends Signature {
    override def print(): String = f"[${id.print()}: $t]: ${annotation.print()}"
  }

  case class MethodSignature(name: Expression, q: String, sig: CallSignature) extends Signature {
    override def print(): String = f"${name.print()}$q ${sig.print()}"
  }

  case class ObjectType(body: List[Signature]) extends Type {
    override def print(): String = f"{${body.print(", ")}}"
  }

  case class TypeAliasDeclaration(id: Identifier, params: Option[TypeParameters], t: Type) extends Declaration {
    override def print(): String = f"type ${id.print()}${params.print()} = ${t.print()}"
  }

  case class Arguments(typeArgs: Option[TypeArguments], args: List[Expression]) extends AST {
    override def print(): String = f"${typeArgs.print()}(${args.print(", ")})"
  }

  case class EnumDeclaration(const: String, id: Identifier, members: List[EnumMember]) extends Declaration {
    override def print(): String = f"$const ${id.print()} { ${members.print(", ")} }"
  }

  case class EnumMember(name: Expression, e: Option[Expression]) extends AST {
    override def print(): String = e match {
      case Some(value) => f"${name.print()} = ${value.print()}"
      case None => name.print()
    }
  }

  case class FunctionType(tParam: Option[TypeParameters], params: Option[ParameterList], result: Type) extends Type {
    override def print(): String = f"${tParam.print()} (${params.print()}) => ${result.print()}"
  }

  case class InterfaceDeclaration(id: Identifier, tParam: Option[TypeParameters], ext: Option[List[TypeReference]], oT: ObjectType) extends Declaration {
    override def print(): String = ext match {
      case Some(value) => f"interface ${id.print()}${tParam.print()} extends ${value.print(", ")} ${oT.print()}"
      case None => f"interface ${id.print()}${tParam.print()} ${oT.print()}"
    }
  }

  case class ClassDeclaration(id: Option[Identifier], tParam: Option[TypeParameters], ext: Option[TypeReference], imp: Option[List[TypeReference]], body: List[ClassElement]) extends Declaration {
    override def print(): String = {
      val extS = ext match {
        case Some(value) => f"extends ${value.print()} "
        case None => ""
      }
      val impS = imp match {
        case Some(value) => f"implements ${value.print(", ")} "
        case None => ""
      }

      f"class ${id.print()}${tParam.print()} $extS$impS{ ${body.print("\n")} }"
    }
  }

  sealed trait ClassElement extends AST

  case class ConstructorDeclaration(modifier: Option[String], params: Option[ParameterList], body: Option[FunctionBody]) extends ClassElement {
    override def print(): String = f"${optStringToString(modifier)}constructor (${params.print()}) ${body.print()}"
  }

  case class IndexMemberDeclaration(is: IndexSignature) extends ClassElement {
    override def print(): String = f"${is.print()};"
  }

  sealed trait PropertyMemberDeclaration extends ClassElement

  case class MemberVariableDeclaration(modifier: Option[String], static: Option[String], name: Expression, annotation: Option[Type], init: Option[Expression]) extends PropertyMemberDeclaration {
    override def print(): String = {
      val (m, a) = modifierAnnotationToString(modifier, annotation)
      init match {
        case Some(value) => f"$m${optStringToString(static)}${name.print()}$a = ${value.print()};"
        case None => f"$m${optStringToString(static)}${name.print()}$a;"
      }
    }
  }

  case class MemberFunctionDeclaration(modifier: Option[String], name: Expression, call: CallSignature, body: Option[FunctionBody]) extends PropertyMemberDeclaration {
    override def print(): String = f"${optStringToString(modifier)}${name.print()} ${call.print()} ${body.print()}"
  }

  // no decorator support
//  case class ClassDeclaration(id: Option[Identifier], tParam: Option[TypeParameters], ) extends Declaration

//  case class

  private def optStringToString(modifier: Option[String]): String = modifier match {
    case Some(value) => f"$value "
    case None => ""
  }

  private def modifierAnnotationToString(modifier: Option[String], annotation: Option[Type]): (String, String) = {
    val a = annotation match {
      case Some(value) => f": ${value.print()}"
      case None => ""
    }

    (optStringToString(modifier), a)
  }

//  case class MethodProperty(name: Expression, call: CallSignature, body: StatementList) extends PropertyDefinition {
//    override def print(): String = f"${name.print()} ${call.print()} { ${body.print()} }"
//  }
  private implicit class OptionBaseASTExtension(o: Option[AST]) {
    def print(): String = o match {
      case Some(value) => value.print()
      case None => ""
    }
  }

  private implicit class ListBaseASTExtension(ls: List[AST]) {
    def print(): String = print("")
    def print(sep: String): String = print("", sep, "")
    def print(start: String, sep: String, end: String): String = ls.map(_.print()).mkString(start, sep, end)
  }
}
