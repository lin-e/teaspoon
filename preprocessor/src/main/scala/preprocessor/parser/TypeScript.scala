package preprocessor.parser

import parsley.Parsley._
import parsley.{token => _, _}
import parsley.character._
import parsley.combinator._
import parsley.expr._
import parsley.implicits.character._
import parsley.lift._

import ASTNodes._
import ParsleyExtensions._
import preprocessor.Common._

object TypeScript {
  private val identifierStart = upper <|> lower <|> '_'
  private val identifierPart = identifierStart <|> digit

  private def mS[A](xs: List[A]): String = xs.mkString

  private def tS(x: Char): String = x.toString

  private def sT(s: String): Parsley[String] = attempt(string(s)) <* trim

  private def kT(s: String): Parsley[String] = attempt(string(s) <* notFollowedBy(identifierStart)) <* trim

  private def cT(s: Char): Parsley[Char] = attempt(char(s)) <* trim

  private def token[A](p: Parsley[A]): Parsley[A] = attempt(p <* notFollowedBy(identifierStart)) <* trim

  private def flt[A](as: List[List[A]]): List[A] = as.flatten

  private def tL[A](o: Option[A]): List[A] = o.toList

  private def oFlt[A](o: Option[List[A]]): List[A] = o match {
    case Some(value) => value
    case None => List()
  }

  private def nStringCharacter(quotes: Char): Parsley[String] = attemptChoice(
    (sourceCharacter butNot attemptChoice(oneOf(quotes, '\\'), lineTerminator)) map tS,
    ("\\" : Parsley[String]) <++> escapeSequence,
    lineContinuation
  )

  private def baseLiteral(i: String, digit: Parsley[Char]): Parsley[String] =
    ("0": Parsley[String]) <++> (i <|> i.toUpperCase) <++> (manyN(1, digit) map mS)

  private def toInfixNFB(nfb: String*)(o: String) = {
    val pNFB = attemptChoice(nfb map sT: _*)
    attempt((sT(o) <* notFollowedBy(pNFB)) #> { (e1, e2) => BinaryOpExpr(e1, e2, o) })
  }
  private def toInfix(o: String) = sT(o) #> { (e1, e2) => BinaryOpExpr(e1, e2, o) }

  private def arrayLike[A](single: Parsley[A], seps: Parsley[List[A]], end: Parsley[A]): Parsley[List[A]] = attemptChoice(
    single <::>
      (many(attempt(seps <:+> single)) map flt) <:::>
      (option(attempt(seps <:+> end)) map oFlt),
    end map { List(_) }
  )

  // check these
  private val sourceCharacter = anyChar
  private val lineTerminator = endOfLine
  private val lineContinuation = ("\\": Parsley[String]) <++> (lineTerminator map tS)

  private val singleLineComment = attempt(string("//")) *> manyUntil(sourceCharacter, lineTerminator <|> eof)
  private val multiLineComment = attempt(string("/*")) *> manyUntil(sourceCharacter, attempt(string("*/")))
  private val trim = many(whitespace <|> attemptChoice(singleLineComment, multiLineComment))

  private val rawKeywords = Seq(
    "break", "case", "catch", "class", "const", "continue", "debugger", "default", "delete", "do", "else", "export",
    "extends", "finally", "for", "function", "if", "import", "in", "instanceof", "new", "return", "super", "switch",
    "this", "throw", "try", "typeof", "var", "void", "while", "with", "yield",
    "enum", "await", "implements", "interface", "package", "private", "protected", "public", "let", // todo
    "true", "null", "false",
//    "lazy", // custom
    "any", "number", "boolean", "string", "symbol", "void"
  )

  private val predefinedType = attemptChoice(
    kT("any") #> AnyType,
    kT("number") #> NumberType,
    kT("boolean") #> BooleanType,
    kT("string") #> StringType,
    kT("symbol") #> SymbolType,
    kT("void") #> VoidType
  )

  private lazy val typeP: Parsley[Type] = attemptChoice(
    functionType,
    uipType
  )

  private lazy val functionType = lift3(FunctionType,
    option(typeParameters),
    sT("(") *> option(parameterList) <* sT(")"),
    sT("=>") *> typeP
  )

  private lazy val typeReference = lift2(TypeReference, typeName, option(notFollowedBy(lineTerminator) *> typeArguments))

  private lazy val objectType = (sT("{") *> sepEndBy(typeMember, sT(",") <|> sT(";")) <* sT("}")) map ObjectType

  private lazy val typeAtoms = attemptChoice(
    (sT("(") *> typeP <* sT(")")) map ParenthesisedType,
    predefinedType,
    literal,
    typeReference,
    objectType,
    (sT("[") *> sepBy1(typeP, sT(",")) <* sT("]")) map TupleType,
    (kT("typeof") *> identifierReference <::> many(sT(".") *> identifierName)) map TypeQuery,
    kT("this") #> ThisType
  )

  // left recursion on primaryType for array case
  private lazy val primaryType = precedence(token(typeAtoms))(
    Ops(Postfix)(notFollowedBy(lineTerminator) *> sT("[") *> sT("]") #> ArrayType)
  )

  // union / intersection / primary type
  private lazy val uipType = precedence(token(primaryType))(
    Ops(InfixL)(sT("&") #> IntersectionType),
    Ops(InfixL)(sT("|") #> UnionType),
  )

  private lazy val typeParameter = lift2(TypeParameter, bindingIdentifier, option(kT("extends") *> typeP))
  private lazy val typeParameters = (sT("<") *> sepBy1(typeParameter, sT(",")) <* sT(">")) map TypeParameters
  private lazy val typeArguments = (sT("<") *> sepBy1(typeP, sT(",")) <* sT(">")) map TypeArguments


  // am I wrong? the definition is identical
  private lazy val typeName = sepBy1(identifierReference, sT("."))

  private lazy val accessModifier = attemptChoice(kT("public"), kT("private"), kT("protected"))
  private lazy val bindingIdentifierOrPattern = bindingIdentifier <|> bindingPattern

  private lazy val requiredParameter = attemptChoice(
    lift2(RequiredIdentifierWithString, bindingIdentifier, sT(":") *> stringLiteral),
    lift3(RequiredIdentifierOrPattern, option(accessModifier), bindingIdentifierOrPattern, option(sT(":") *> typeP))
  )

  private lazy val requiredParameterList = sepBy1(requiredParameter, sT(","))

  private lazy val optionalParameter = attemptChoice(
    lift2(OptionalIdentifierWithString, bindingIdentifier <* sT("?"), sT(":") *> stringLiteral),
    lift3(OptionalIdentifierOrPattern,
      option(accessModifier),
      bindingIdentifierOrPattern <* sT("?"),
      option(sT(":") *> typeP)),
    lift4(OptionalIdentifierOrPatternInit,
      option(accessModifier),
      bindingIdentifierOrPattern,
      option(sT(":") *> typeP),
      sT("=") *> singleExpression)
  )

  private lazy val optionalParameterList = sepBy1(optionalParameter, sT(","))

  private lazy val restParameter = lift2(RestSpreadParameter, sT("...") *> bindingIdentifier, option(sT(":") *> typeP))

  // consider the ordering here (may have ambiguity)?
  private lazy val parameterList = attemptChoice(
    (optionalParameterList <* sT(",")) <:+> restParameter,
    (requiredParameterList <* sT(",")) <:::> (optionalParameterList <* sT(",")) <:+> restParameter,
    (requiredParameterList <* sT(",")) <:::> optionalParameterList,
    (requiredParameterList <* sT(",")) <:+> restParameter,
    optionalParameterList,
    requiredParameterList,
    restParameter map { List(_) }
  ) map ParameterList

  private lazy val callSignature = lift3(CallSignature,
    option(typeParameters),
    sT("(") *> option(parameterList) <* sT(")"),
    option(sT(":") *> typeP))
  private lazy val indexSignature = lift3(IndexSignature,
    sT("[") *> bindingIdentifier,
    sT(":") *> (kT("string") <|> kT("number")) <* sT("]"),
    sT(":") *> typeP)

  private lazy val typeMember = attemptChoice(
    lift3(MethodSignature, propertyName, sT("?").opt(), callSignature),
    lift3(PropertySignature, propertyName, sT("?").opt(), option(sT(":") *> typeP)),
    callSignature,
    lift3(ConstructSignature,
      kT("new") *> option(typeParameters),
      sT("(") *> option(parameterList) <* sT(")"),
      option(sT(":") *> typeP)),
    indexSignature
  )

  // null literal
  private lazy val nullLiteral = kT("null") map Literal

  // boolean literals
  private lazy val booleanLiteral = kT("true") <|> kT("false") map Literal

  // numeric literals
  private lazy val decimalDigits = manyN(1, digit) map mS
  private lazy val signedInteger = ("+" <|> "-" <|> "") <++> decimalDigits
  private lazy val exponentPart = ("e" <|> "E") <++> signedInteger

  private lazy val decimalLiteral = attemptChoice(
    decimalDigits <++> "." <++> decimalDigits.opt() <++> exponentPart.opt(),
    (".": Parsley[String]) <++> decimalDigits <++> exponentPart.opt(),
    decimalDigits <++> exponentPart.opt()
  )

  private lazy val binaryIntegerLiteral = baseLiteral("b", '0' <|> '1')
  private lazy val octalIntegerLiteral = baseLiteral("o", octDigit)
  private lazy val hexIntegerLiteral = baseLiteral("x", hexDigit)

  // identifier - TODO: very incorrect?
  private lazy val identifier = token(((identifierStart <::> many(identifierPart)) map mS map Identifier) butNot attemptChoice(rawKeywords map kT: _*))
  private lazy val identifierName = token((identifierStart <::> many(identifierPart)) map mS map Identifier)
  private lazy val identifierReference = token((kT("yield") #> Yield: Parsley[Expression]) <|> identifier)

  // regex literals (lol)
  private val regexNonTerminator = (sourceCharacter butNot lineTerminator) map tS
  private val regexBackslashSeq = ("\\": Parsley[String]) <++> regexNonTerminator
  private val regexClassChar = choice(regexBackslashSeq, regexNonTerminator)
  private val regexClass = ("[": Parsley[String]) *> (manyUntil(regexClassChar, "]") map mS) map { x => f"[$x]" }
  private val regexChar = attemptChoice(
    regexClass,
    regexBackslashSeq,
    regexNonTerminator butNot attempt("/")
  )
  private lazy val regexLiteral = (("/": Parsley[String]) <++> (many(regexChar) map mS) <++> ("/": Parsley[String]) <++> (many(identifierPart) map mS)) map Literal

  // string literals
  private lazy val hexEscapeSequence = ("x": Parsley[String]) <++> (repeat(2, hexDigit) map mS)
  private lazy val unicodeEscapeSequence = ("u": Parsley[String]) <++> attemptChoice(
    repeat(4, hexDigit) map mS,
    ("{": Parsley[String]) <++> (manyN(1, hexDigit) map mS) <++> "}"
  )

  private lazy val singleEscapeCharacter = oneOf('\'', '"', '\\', 'b', 'f', 'n', 'r', 't', 'v')
  private lazy val escapeCharacter = attemptChoice(singleEscapeCharacter, digit, 'x', 'u')
  private lazy val nonEscapeCharacter = satisfy({
    case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => false
    case '\'' | '"' | '\\' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' => false
    case 'x' | 'u' => false
    case _ => true
  })
  private lazy val characterEscapeSequence = attemptChoice(singleEscapeCharacter, nonEscapeCharacter)

  private lazy val escapeSequence = attemptChoice(
    characterEscapeSequence map tS,
    ("0": Parsley[String]) <* notFollowedBy(digit),
    hexEscapeSequence,
    unicodeEscapeSequence
  )

  private lazy val doubleStringCharacter = nStringCharacter('"')
  private lazy val singleStringCharacter = nStringCharacter('\'')
  private lazy val stringLiteral = attemptChoice(
    ("\"": Parsley[String]) <++> (many(doubleStringCharacter) map mS) <++> "\"",
    ("'": Parsley[String]) <++> (many(singleStringCharacter) map mS) <++> "'",
  ) map Literal

  // combining literals

  private lazy val numericLiteral = attemptChoice(
    decimalLiteral,
    hexIntegerLiteral,
    octalIntegerLiteral,
    binaryIntegerLiteral
  ) map Literal

  private lazy val literal = attemptChoice(
    nullLiteral,
    booleanLiteral,
    stringLiteral,
    // templateStringLiteral, TODO
    regexLiteral,
    numericLiteral
  )

  private lazy val postfix = Seq("++", "--") map { o => sT(o) #> { e => PostfixExpr(e, o) } }
  private lazy val kwPrefix = Seq("delete", "void", "typeof") map { k => kT(k) #> { e => KeywordPrefixExpr(e, k) } }
  private lazy val prefix = Seq("++", "--", "+", "-", "~", "!") map { o => sT(o) #> { e => PrefixExpr(e, o) } }

  private lazy val multiplicativeInfix = Seq("*", "/", "%") map toInfixNFB("=")
  private lazy val additiveInfix = Seq("+", "-") map toInfixNFB("=")
  private lazy val bitShiftInfix = Seq(">>>", "<<", ">>") map toInfixNFB("=")
  private lazy val relationalInfix = Seq("<=", ">=", "<", ">") map toInfix
  private lazy val relationalKeywordInfix = Seq("instanceof", "in") map { o => kT(o) #> { (e1, e2) => BinaryOpExpr(e1, e2, o) }}
  private lazy val equalityInfix = Seq("===", "!===", "==", "!=") map toInfix
  private lazy val assignmentOperatorInfix = Seq("*", "/", "%", "+", "-", ">>>", "<<", ">>", "&", "^", "!") map { o => toInfix(o + "=") }

  // why is this allowed? :(
  // arrays etc.
  private lazy val spread = Spread <#> (sT("...") *> identifier)
  private lazy val elision = sT(",") #> Elision

  private lazy val elisionSep = manyN(1, elision)

  private lazy val elementList = arrayLike(singleExpression, elisionSep, spread)

  private lazy val arrayLiteral = (sT("[") *> (
    many(elision: Parsley[Expression]) <:::>
      (option(elementList) map oFlt) <:::>
      many(elision)
    ) <* sT("]")) map ArrayLiteral

  private lazy val propertyName = token(attemptChoice(
    identifierName,
    stringLiteral,
    numericLiteral,
    (sT("[") *> singleExpression <* sT("]")) map ComputedPropertyName
  ))
  private lazy val propertyDefinition: Parsley[PropertyDefinition] = attemptChoice(
    lift2(CoverInitialisedName, identifierReference, sT("=") *> singleExpression),
    lift2(PropertyExpressionAssignment, propertyName, sT(":") *> singleExpression),
    lift3(MethodProperty, propertyName, callSignature, sT("{") *> statementListOpt <* sT("}")),
    identifier
    // TODO (see above)
  )
  private lazy val objectLiteral = (sT("{") *> sepEndBy(propertyDefinition, sT(",")) <* sT("}")) map ObjectLiteral

  private lazy val arguments = lift2(Arguments,
    option(typeArguments),
    sT("(") *> (option(argumentList) map oFlt) <* sT(")"))
  private lazy val argumentList = attemptChoice(
    sepBy1(singleExpression, sT(",")) <:::> (option(sT(",") *> spread) map tL),
    spread map { s => List(s) }
  )

  private lazy val callExpressionAtoms = attemptChoice(
    lift2(MemberCall, memberExpression, arguments),
    (kT("super") *> arguments) map SuperCall
  )
  private lazy val callExpression = precedence(token(callExpressionAtoms))(
    Ops(Postfix)(attempt(arguments) map { args => c => NestedCall(c, args)}),
    Ops(Postfix)(attempt(sT("[") *> expressionSequence <* sT("]")) map { es => c => IndexedCall(c, es)}), // maybe expression sequence?
    Ops(Postfix)(attempt(sT(".") *> identifierName) map { i => c => DotCall(c, i)})
  )

  private lazy val memberExpressionAtoms = attemptChoice(
    kT("this") #> This,
    kT("yield") #> Yield,
    parenthesisedExpr,
    identifier,
    literal,
    arrayLiteral,
    objectLiteral,
    functionExpression,
    (kT("new") *> memberExpression) map NewMemberExpr,
    (kT("new") *> sT(".") *> kT("target")) #> MetaProperty,
    (kT("super") *> sT(".") *> identifierName) map SuperPropertyDot,
    (kT("super") *> sT("[") *> singleExpression <* sT("]")) map SuperPropertyIndexed,
  )
  private lazy val memberExpression: Parsley[Expression] = precedence(token(memberExpressionAtoms))(
    Ops(Postfix)(attempt(sT("[") *> singleExpression <* sT("]")) map { e => m => IndexedMember(m, e)}), // maybe expression sequence?
    Ops(Postfix)(attempt(sT(".") *> identifierName) map { i => m => DotMember(m, i)})
    // TODO template
  )

  private lazy val functionExpression = lift3(FunctionExpression,
    kT("function") *> option(bindingIdentifier),
    callSignature,
    sT("{") *> statementListOpt <* sT("}"))

  private lazy val functionBody = (sT("{") *> statementListOpt <* sT("}")) map FunctionBody

  private lazy val conciseBody = attemptChoice(
    singleExpression,
    functionBody
  )

  private lazy val arrowFunction = attemptChoice(
    lift2(ArrowFunctionIdentifier, bindingIdentifier, sT("=>") *> conciseBody),
    lift2(ArrowFunctionParams, callSignature, sT("=>") *> conciseBody)
  )

  private lazy val parenthesisedExpr = (sT("(") *> expressionSequence <* sT(")")) map { // Convert singleton to an expression
    case ExpressionSequence(e :: Nil) => e
    case es => ParenthesisedExpr(es)
  }

  private lazy val atoms: Parsley[Expression] = attemptChoice(
    functionExpression,
    // TODO: class
    kT("this") #> This,
    kT("super") #> Super,
    (kT("new") *> memberExpression) map NewMemberExpr,
    callExpression,
    literal,
    arrowFunction,
    identifier,
    arrayLiteral,
    objectLiteral,
    parenthesisedExpr,
  )

  private lazy val expressionSequence = sepBy1(singleExpression, sT(",")) map ExpressionSequence

  private lazy val beforeCustom: Seq[Ops[Expression, Expression]] = Seq(
    Ops(Postfix)(attempt(sT("[") *> expressionSequence <* sT("]")) map { es => e => MemberIndexExpr(e, es) }),
    Ops(Postfix)(attempt(sT(".") *> identifierName) map { i => e => MemberDotExpr(e, i) }),
    Ops(Postfix)(postfix: _*),
    Ops(Prefix)(kwPrefix: _*),
    Ops(Prefix)(prefix: _*),
    Ops(Prefix)(attempt(sT("<") *> typeP <* sT(">")) map { t => e => TypeCastExpr(t, e)}),
  )

  private lazy val infixFunctionCall: Ops[Expression, Expression] =
    Ops(InfixL)(attempt(sT("<") *> identifier <* sT(">")) map { f => (a, b) => InfixFuncExpr(a, b, f) })
//  private lazy val infixFunctionCall: Ops[Expression, Expression] =
//    Ops(InfixL)(attempt(sT("<") *> identifier <* sT(">")) map { f => (a, b) => MemberCall(f, Arguments(None, List(a, b))) })

  private lazy val customOps: Seq[Ops[Expression, Expression]] = customOperators
    .groupBy { case (_, CustomOperator(_, precedence, _)) => precedence }
    .toSeq
    .sortBy { case (precedence, _) => precedence }
    .reverse
    .map { case (_, ops) => Ops(InfixL)(ops.keys.toSeq map toInfix: _*) }

  private lazy val afterCustom: Seq[Ops[Expression, Expression]] = Seq(
    Ops(InfixL)(multiplicativeInfix: _*),
    Ops(InfixL)(additiveInfix: _*),
    Ops(InfixL)(bitShiftInfix: _*),
    Ops(InfixL)(relationalInfix: _*),
    Ops(InfixL)(relationalKeywordInfix: _*),
    Ops(InfixL)(equalityInfix: _*),
    Ops(InfixL)(toInfixNFB("&", "=")("&")),
    Ops(InfixL)(toInfixNFB("=")("^")),
    Ops(InfixL)(toInfixNFB("|", "=")("|")),
    Ops(InfixL)(toInfix("&&")),
    Ops(InfixL)(toInfix("||")),
    Ops(InfixL)(attempt(sT("?") *> singleExpression <* sT(":")) map { e1 => (p, e2) => TernaryExpr(p, e1, e2)}),
    Ops(InfixL)(toInfixNFB(">")("=")),
    Ops(InfixL)(assignmentOperatorInfix: _*),
    // TODO: TemplateStringLiteral,
  )

  private lazy val singleExpression = precedence(token(atoms))(beforeCustom ++ (customOps :+ infixFunctionCall) ++ afterCustom: _*)

  private lazy val bindingElision: Parsley[BindingElement] = sT(",") #> BindingElision
  private lazy val bindingElisionSep = manyN(1, bindingElision)
  private lazy val bindingIdentifier = identifier
  private lazy val singleNameBinding = lift2(SingleNameBinding, bindingIdentifier, option(sT("=") *> singleExpression))
  private lazy val bindingSpread = (sT("...") *> bindingIdentifier) map BindingSpread

  private lazy val bindingElementList = arrayLike(bindingElement, bindingElisionSep, bindingSpread)

  private lazy val bindingPattern = attemptChoice(bindingObject, bindingArray)
  private lazy val bindingArray = (sT("[") *> (
    many(bindingElision) <:::>
      (option(bindingElementList) map oFlt) <:::>
      many(bindingElision)
    ) <* sT("]")) map BindingArray
  private lazy val bindingObject = (sT("{") *> sepEndBy(bindingProperty, sT(",")) <* sT("}")) map BindingObject
  private lazy val bindingProperty = attemptChoice(
    singleNameBinding,
    lift2(BindingPropertyName, propertyName, sT(":") *> bindingElement)
  )
  private lazy val bindingElement: Parsley[BindingElement] = attemptChoice(
    singleNameBinding,
    lift2(BindingPatternElement, bindingPattern, option(sT("=") *> singleExpression))
  )

  private lazy val eos = sT(";")

  private lazy val variableDeclaration = attemptChoice(
    lift3(VariableDeclaration, bindingIdentifier, option(sT(":") *> typeP), option(sT("=") *> singleExpression)),
    lift3(VariableDeclaration, bindingPattern, option(sT(":") *> typeP), option(sT("=") *> singleExpression))
  )
  private lazy val variableDeclarationList = sepBy1(variableDeclaration, sT(","))

  // OWN
  private lazy val lazyVariableDeclaration = lift3(LazyVariableStatement,
    kT("lazy") *> bindingIdentifier,
    option(sT(":") *> typeP),
    sT("=") *> singleExpression <* eos)

  private lazy val inlineFunctionDeclaration = lift3(InlineFunctionStatement,
    kT("inline") *> bindingIdentifier,
    sT("(") *> sepBy(bindingIdentifier, sT(",")) <* sT(")"),
    sT("=") *> singleExpression <* eos)

  private lazy val inlineVariableDeclaration = lift3(InlineVariableStatement,
    kT("inline") *> bindingIdentifier,
    option(sT(":") *> typeP),
    sT("=") *> singleExpression <* eos)

  private lazy val statementListItem = attemptChoice(functionDeclaration, statement)
  private lazy val statementListOpt: Parsley[StatementList] = many(statementListItem) map StatementList

  private lazy val varModifier = attemptChoice(kT("var"), kT("let"), kT("const"), kT("val"))

  private lazy val iterationStatement = attemptChoice(
    lift2(DoStatement, kT("do") *> statement, kT("while") *> sT("(") *> expressionSequence <* sT(")") <* eos),
    lift2(WhileStatement, kT("while") *> sT("(") *> expressionSequence <* sT(")"), statement),
    lift4(ForStatement,
      kT("for") *> sT("(") *> option(expressionSequence),
      sT(";") *> option(expressionSequence),
      sT(";") *> option(expressionSequence) <* sT(")"),
      statement),
    lift5(ForVarStatement,
      kT("for") *> sT("(") *> varModifier,
      variableDeclarationList,
      sT(";") *> option(expressionSequence),
      sT(";") *> option(expressionSequence) <* sT(")"),
      statement),
    lift4(ForInStatement,
      kT("for") *> sT("(") *> singleExpression,
      kT("in") <|> kT("of"),
      expressionSequence <* sT(")"),
      statement),
    lift5(ForVarInStatement,
      kT("for") *> sT("(") *> varModifier,
      variableDeclaration,
      kT("in") <|> kT("of"),
      expressionSequence <* sT(")"),
      statement),
  )

  private lazy val caseClause: Parsley[SwitchClause] = lift2(CaseClause, kT("case") *> expressionSequence <* sT(":"), statementListOpt)
  private lazy val defaultClause = (kT("default") *> sT(":") *> statementListOpt) map DefaultClause
  private lazy val clauses = many(caseClause) <:::> (option(defaultClause) map tL) <:::> many(caseClause)

  private lazy val block = (sT("{") *> statementListOpt <* sT("}")) map BlockStatement

  private lazy val catchProduction = lift2(CatchProduction,
    kT("catch") *> sT("(") *> attemptChoice(bindingIdentifier, bindingPattern) <* sT(")"),
    block)
  private lazy val finallyProduction = (kT("finally") *> block) map FinallyProduction
  private lazy val tryStatement = attemptChoice(
    lift3(TryCatchStatement, kT("try") *> block, catchProduction, option(finallyProduction)),
    lift2(TryFinallyStatement, kT("try") *> block, finallyProduction)
  )

  private lazy val variableStatement = lift2(VariableStatement, varModifier, variableDeclarationList <* eos)

  private lazy val statement: Parsley[Statement] = attemptChoice(
    block,
    lazyVariableDeclaration,
    inlineFunctionDeclaration,
    inlineVariableDeclaration,
    variableStatement,
    sT(";") #> EmptyStatement,
    // todo: notOpenBraceAndNotFunction
    expressionSequence <* eos,
    lift3(IfStatement, kT("if") *> sT("(") *> expressionSequence <* sT(")"), statement, option(kT("else") *> statement)),
    iterationStatement,
    (kT("continue") *> option(notFollowedBy(lineTerminator) *> identifier) <* eos) map ContinueStatement,
    (kT("break") *> option(notFollowedBy(lineTerminator) *> identifier) <* eos) map BreakStatement,
    (kT("return") *> option(notFollowedBy(lineTerminator) *> expressionSequence) <* eos) map ReturnStatement,
    lift2(WithStatement, kT("with") *> sT("(") *> expressionSequence <* sT(")"), statement),
    lift2(LabelledStatement, token(identifier) <* sT(":"), statement),
    lift2(SwitchStatement, kT("switch") *> sT("(") *> expressionSequence <* sT(")"), kT("{") *> clauses <* kT("}")),
    (kT("throw") *> notFollowedBy(lineTerminator) *> expressionSequence <* eos) map ThrowStatement,
    tryStatement,
    (kT("debugger") <* eos) #> DebuggerStatement
  )

  private lazy val functionDeclaration = attemptChoice(
    lift3(NonEmptyFunctionDeclaration,
      kT("function") *> option(bindingIdentifier),
      callSignature,
      sT("{") *> statementListOpt <* sT("}")),
    lift2(EmptyFunctionDeclaration,
      kT("function") *> option(bindingIdentifier),
      callSignature <* eos)
  )

  private lazy val typeAliasDeclaration = lift3(TypeAliasDeclaration,
    kT("type") *> bindingIdentifier,
    option(typeParameters),
    kT("=") *> typeP <* eos
  )

  private lazy val enumDeclaration = lift3(EnumDeclaration,
    kT("const").opt(),
    kT("enum") *> bindingIdentifier,
    sT("{") *> sepEndBy(enumMember, sT(",")) <* sT("}"))
  private lazy val enumMember = lift2(EnumMember, propertyName, option(sT("=") *> singleExpression))

  private lazy val importBindingClause = bindingIdentifier map ImportBindingClause
  private lazy val nameSpaceImportClause = (sT("*") *> kT("as") *> bindingIdentifier) map NameSpaceImportClause
  private lazy val namedImportsClause = (sT("{") *> sepEndBy(importSpecifier, sT(",")) <* sT("}")) map NamedImportsClause
  private lazy val importSpecifier = lift2(ImportSpecifier, option(attempt(identifierName <* kT("as"))), bindingIdentifier)
  private lazy val bindingFollowedClause = lift2(BindingFollowedClause,
    importBindingClause <* sT(","),
    nameSpaceImportClause <|> namedImportsClause)

  private lazy val importClause = attemptChoice(
    bindingFollowedClause, // do this first
    importBindingClause,
    nameSpaceImportClause,
    namedImportsClause
  )

  private lazy val importDeclaration = attemptChoice(
    lift2(ImportFrom, kT("import") *> importClause, kT("from") *> stringLiteral <* eos),
    lift2(ImportRequire, kT("import") *> bindingIdentifier <* sT("="), kT("require") *> sT("(") *> stringLiteral <* sT(")") <* eos),
    (kT("import") *> stringLiteral <* eos) map ImportModuleSpecifier
  )

  private lazy val exportSpecifier = lift2(ExportSpecifier, identifierName, option(kT("as") *> identifierName)) // isn't this the same?
  private lazy val exportClause = (sT("{") *> sepEndBy(exportSpecifier, sT(",")) <* sT("}")) map ExportClause

  private lazy val exportDeclaration = attemptChoice(
    (kT("export") *> sT("*") *> kT("from") *> stringLiteral <* eos) map ExportStar,
    lift2(ExportWithClause, kT("export") *> exportClause, option(kT("from") *> stringLiteral) <* eos),
    (kT("export") *> variableStatement) map ExportVar,
    (kT("export") *> functionDeclaration) map ExportFun
  )

  private val moduleItem = attemptChoice(
    classDeclaration,
    interfaceDeclaration,
    importDeclaration,
    exportDeclaration,
    typeAliasDeclaration,
    enumDeclaration,
    statementListItem,
  )

  private lazy val classDeclaration = lift5(ClassDeclaration,
    kT("class") *> option(bindingIdentifier),
    option(typeParameters),
    option(kT("extends") *> typeReference),
    option(kT("implements") *> sepBy1(typeReference, sT(","))),
    kT("{") *> many(classElement) <* kT("}")
  )

  private lazy val classElement: Parsley[ClassElement] = attemptChoice(
    lift3(ConstructorDeclaration, option(accessModifier), kT("constructor") *> sT("(") *> option(parameterList) <* sT(")"), option(functionBody)),
    propertyMemberDeclaration,
    (indexSignature <* eos) map IndexMemberDeclaration
  )

  private lazy val interfaceDeclaration = lift4(InterfaceDeclaration,
    kT("interface") *> bindingIdentifier,
    option(typeParameters),
    option(kT("extends") *> sepBy1(typeReference, sT(","))),
    objectType)

  private lazy val propertyMemberDeclaration = attemptChoice(
    lift5(MemberVariableDeclaration, option(accessModifier), option(kT("static")), propertyName, option(sT(":") *> typeP), option(sT("=") *> singleExpression) <* eos),
    lift4(MemberFunctionDeclaration, option(accessModifier), propertyName, callSignature, option(functionBody))
  )

  val module: Parsley[Module] = (trim *> many(moduleItem) <* trim <* eof) map Module
}
