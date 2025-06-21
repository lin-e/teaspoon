package preprocessor.optimiser

import IRNodes._
import preprocessor.parser.ASTNodes._

trait PipelineStage {
  def run(m: Module)(implicit state: IRState): Module = process(m)

  // AST traversal

  def process(l: Literal)(implicit state: IRState): Literal = l

  def process(i: Identifier)(implicit state: IRState): Identifier = i

  def process(al: ArrayLiteral)(implicit state: IRState): ArrayLiteral = ArrayLiteral(al.elems.map(processE))

  def process(ol: ObjectLiteral)(implicit state: IRState): ObjectLiteral = ObjectLiteral(ol.defs.map(processPD))

  def process(s: Spread)(implicit state: IRState): Spread = Spread(process(s.id))

  def process(m: Module)(implicit state: IRState): Module = Module(m.body.map(processMI))

  def process(ims: ImportModuleSpecifier)(implicit state: IRState): ImportModuleSpecifier = ImportModuleSpecifier(process(ims.specifier))

  def process(ifrom: ImportFrom)(implicit state: IRState): ImportFrom = ImportFrom(processIC(ifrom.clause), process(ifrom.specifier))

  def process(ir: ImportRequire)(implicit state: IRState): ImportRequire = ImportRequire(process(ir.binding), process(ir.specifier))

  def process(ibc: ImportBindingClause)(implicit state: IRState): ImportBindingClause = ImportBindingClause(process(ibc.binding))

  def process(nsic: NameSpaceImportClause)(implicit state: IRState): NameSpaceImportClause = NameSpaceImportClause(process(nsic.binding))

  def process(nic: NamedImportsClause)(implicit state: IRState): NamedImportsClause = NamedImportsClause(nic.specs.map(process))

  def process(is: ImportSpecifier)(implicit state: IRState): ImportSpecifier = ImportSpecifier(is.name.map(process), process(is.binding))

  def process(bfc: BindingFollowedClause)(implicit state: IRState): BindingFollowedClause = BindingFollowedClause(process(bfc.b), processIC(bfc.ns))

  def process(es: ExportStar)(implicit state: IRState): ExportStar = ExportStar(process(es.specifier))

  def process(ewc: ExportWithClause)(implicit state: IRState): ExportWithClause = ExportWithClause(process(ewc.ec), ewc.from.map(process))

  def process(ev: ExportVar)(implicit state: IRState): ExportVar = ExportVar(process(ev.s))

  def process(ef: ExportFun)(implicit state: IRState): ExportFun = ExportFun(processFD(ef.f))

  def process(ec: ExportClause)(implicit state: IRState): ExportClause = ExportClause(ec.specs.map(process))

  def process(es: ExportSpecifier)(implicit state: IRState): ExportSpecifier = ExportSpecifier(process(es.id), es.as.map(process))

  def process(sl: StatementList)(implicit state: IRState): StatementList = StatementList(sl.stats.map(processSLI))

  def process(es: ExpressionSequence)(implicit state: IRState): ExpressionSequence = ExpressionSequence(es.es.map(processE))

  def process(pe: PostfixExpr)(implicit state: IRState): PostfixExpr = PostfixExpr(processE(pe.e), pe.op)

  def process(kpe: KeywordPrefixExpr)(implicit state: IRState): KeywordPrefixExpr = KeywordPrefixExpr(processE(kpe.e), kpe.op)

  def process(pe: PrefixExpr)(implicit state: IRState): PrefixExpr = PrefixExpr(processE(pe.e), pe.op)

  def process(tce: TypeCastExpr)(implicit state: IRState): TypeCastExpr = TypeCastExpr(processT(tce.t), processE(tce.e))

  def process(boe: BinaryOpExpr)(implicit state: IRState): BinaryOpExpr = BinaryOpExpr(processE(boe.e1), processE(boe.e2), boe.op)

  def process(ife: InfixFuncExpr)(implicit state: IRState): InfixFuncExpr = InfixFuncExpr(processE(ife.e1), processE(ife.e1), process(ife.f))

  def process(te: TernaryExpr)(implicit state: IRState): TernaryExpr = TernaryExpr(processE(te.p), processE(te.e1), processE(te.e2))

  def process(mde: MemberDotExpr)(implicit state: IRState): MemberDotExpr = MemberDotExpr(processE(mde.e), process(mde.i))

  def process(mie: MemberIndexExpr)(implicit state: IRState): MemberIndexExpr = MemberIndexExpr(processE(mie.e), process(mie.es))

  def process(pe: ParenthesisedExpr)(implicit state: IRState): ParenthesisedExpr = ParenthesisedExpr(process(pe.es))

  def process(nme: NewMemberExpr)(implicit state: IRState): NewMemberExpr = NewMemberExpr(processE(nme.expr))

  //  def process(e: Elision)(implicit state: IRState): Elision = ???
  //  def process(t: This)(implicit state: IRState): This = ???
  //  def process(s: Super)(implicit state: IRState): Super = ???
  //  def process(y: Yield)(implicit state: IRState): Yield = ???
  def process(pea: PropertyExpressionAssignment)(implicit state: IRState): PropertyExpressionAssignment = PropertyExpressionAssignment(processE(pea.name), processE(pea.e))

  def process(cin: CoverInitialisedName)(implicit state: IRState): CoverInitialisedName = CoverInitialisedName(processE(cin.name), processE(cin.e))

  def process(mp: MethodProperty)(implicit state: IRState): MethodProperty = MethodProperty(processE(mp.name), process(mp.call), process(mp.body))

  def process(cpn: ComputedPropertyName)(implicit state: IRState): ComputedPropertyName = ComputedPropertyName(processE(cpn.e))

  def process(mc: MemberCall)(implicit state: IRState): MemberCall = MemberCall(processE(mc.member), process(mc.args))

  def process(sc: SuperCall)(implicit state: IRState): SuperCall = SuperCall(process(sc.args))

  def process(nc: NestedCall)(implicit state: IRState): NestedCall = NestedCall(processE(nc.call), process(nc.args))

  def process(ic: IndexedCall)(implicit state: IRState): IndexedCall = IndexedCall(processE(ic.call), process(ic.es))

  def process(dc: DotCall)(implicit state: IRState): DotCall = DotCall(processE(dc.call), process(dc.i))

  def process(im: IndexedMember)(implicit state: IRState): IndexedMember = IndexedMember(processE(im.member), processE(im.e))

  def process(dm: DotMember)(implicit state: IRState): DotMember = DotMember(processE(dm.member), process(dm.i))

  //  def process(mp: MetaProperty)(implicit state: IRState): MetaProperty = ???
  def process(spd: SuperPropertyDot)(implicit state: IRState): SuperPropertyDot = SuperPropertyDot(process(spd.i))

  def process(spi: SuperPropertyIndexed)(implicit state: IRState): SuperPropertyIndexed = SuperPropertyIndexed(processE(spi.e))

  //  def process(es: EmptyStatement)(implicit state: IRState): EmptyStatement = ???
  //  def process(ds: DebuggerStatement)(implicit state: IRState): DebuggerStatement = ???
  def process(vs: VariableStatement)(implicit state: IRState): VariableStatement = VariableStatement(vs.modifier, vs.vars.map(process))

  def process(lvs: LazyVariableStatement)(implicit state: IRState): LazyVariableStatement = LazyVariableStatement(processBPatt(lvs.lhs), lvs.annotation.map(processT), processE(lvs.rhs))

  def process(ivs: InlineVariableStatement)(implicit state: IRState): InlineVariableStatement = InlineVariableStatement(processBPatt(ivs.lhs), ivs.annotation.map(processT), processE(ivs.rhs))

  def process(ifs: InlineFunctionStatement)(implicit state: IRState): InlineFunctionStatement = InlineFunctionStatement(process(ifs.lhs), ifs.params.map(process), processE(ifs.rhs))

  def process(vd: VariableDeclaration)(implicit state: IRState): VariableDeclaration = VariableDeclaration(processBPatt(vd.pattern), vd.annotation.map(processT), vd.expr.map(processE))

  def process(bs: BlockStatement)(implicit state: IRState): BlockStatement = BlockStatement(process(bs.stats))

  def process(is: IfStatement)(implicit state: IRState): IfStatement = IfStatement(process(is.p), processStat(is.t), is.f.map(processStat))

  def process(ds: DoStatement)(implicit state: IRState): DoStatement = DoStatement(processStat(ds.s), process(ds.p))

  def process(ws: WhileStatement)(implicit state: IRState): WhileStatement = WhileStatement(process(ws.p), processStat(ws.s))

  def process(fs: ForStatement)(implicit state: IRState): ForStatement = ForStatement(fs.e1.map(process), fs.e2.map(process), fs.e3.map(process), processStat(fs.s))

  def process(fvs: ForVarStatement)(implicit state: IRState): ForVarStatement = ForVarStatement(fvs.modifier, fvs.vars.map(process), fvs.e1.map(process), fvs.e2.map(process), processStat(fvs.s))

  def process(fis: ForInStatement)(implicit state: IRState): ForInStatement = ForInStatement(processE(fis.e), fis.io, process(fis.es), processStat(fis.s))

  def process(fvis: ForVarInStatement)(implicit state: IRState): ForVarInStatement = ForVarInStatement(fvis.modifier, process(fvis.vd), fvis.io, process(fvis.es), processStat(fvis.s))

  def process(cs: ContinueStatement)(implicit state: IRState): ContinueStatement = ContinueStatement(cs.id.map(process))

  def process(bs: BreakStatement)(implicit state: IRState): BreakStatement = BreakStatement(bs.id.map(process))

  def process(rs: ReturnStatement)(implicit state: IRState): ReturnStatement = ReturnStatement(rs.es.map(process))

  def process(ts: ThrowStatement)(implicit state: IRState): ThrowStatement = ThrowStatement(process(ts.es))

  def process(ws: WithStatement)(implicit state: IRState): WithStatement = WithStatement(process(ws.es), processStat(ws.s))

  def process(ss: SwitchStatement)(implicit state: IRState): SwitchStatement = SwitchStatement(process(ss.es), ss.clauses.map(processSC))

  def process(cc: CaseClause)(implicit state: IRState): CaseClause = CaseClause(process(cc.es), process(cc.ss))

  def process(dc: DefaultClause)(implicit state: IRState): DefaultClause = DefaultClause(process(dc.ss))

  def process(ls: LabelledStatement)(implicit state: IRState): LabelledStatement = LabelledStatement(process(ls.id), processStat(ls.s))

  def process(cp: CatchProduction)(implicit state: IRState): CatchProduction = CatchProduction(processBPatt(cp.pattern), processStat(cp.b))

  def process(fp: FinallyProduction)(implicit state: IRState): FinallyProduction = FinallyProduction(processStat(fp.b))

  def process(tcs: TryCatchStatement)(implicit state: IRState): TryCatchStatement = TryCatchStatement(processStat(tcs.b), process(tcs.c), tcs.f.map(process))

  def process(tfs: TryFinallyStatement)(implicit state: IRState): TryFinallyStatement = TryFinallyStatement(processStat(tfs.b), process(tfs.f))

  def process(bo: BindingObject)(implicit state: IRState): BindingObject = BindingObject(bo.props.map(processBProp))

  def process(ba: BindingArray)(implicit state: IRState): BindingArray = BindingArray(ba.elems.map(processBE))

  //  def process(be: BindingElision)(implicit state: IRState): BindingElision = ???
  def process(snb: SingleNameBinding)(implicit state: IRState): SingleNameBinding = SingleNameBinding(processBPatt(snb.id), snb.init.map(processE))

  def process(bpe: BindingPatternElement)(implicit state: IRState): BindingPatternElement = BindingPatternElement(processBPatt(bpe.pattern), bpe.init.map(processE))

  def process(bpn: BindingPropertyName)(implicit state: IRState): BindingPropertyName = BindingPropertyName(processE(bpn.property), processBE(bpn.element))

  def process(bs: BindingSpread)(implicit state: IRState): BindingSpread = BindingSpread(process(bs.id))

  def process(nefd: NonEmptyFunctionDeclaration)(implicit state: IRState): NonEmptyFunctionDeclaration = NonEmptyFunctionDeclaration(nefd.id.map(process), process(nefd.sig), process(nefd.body))

  def process(fe: FunctionExpression)(implicit state: IRState): FunctionExpression = FunctionExpression(fe.id.map(process), process(fe.sig), process(fe.body))

  def process(efd: EmptyFunctionDeclaration)(implicit state: IRState): EmptyFunctionDeclaration = EmptyFunctionDeclaration(efd.id.map(process), process(efd.sig))

  def process(fb: FunctionBody)(implicit state: IRState): FunctionBody = FunctionBody(process(fb.body))

  def process(afi: ArrowFunctionIdentifier)(implicit state: IRState): ArrowFunctionIdentifier = ArrowFunctionIdentifier(process(afi.id), processCB(afi.body))

  def process(afp: ArrowFunctionParams)(implicit state: IRState): ArrowFunctionParams = ArrowFunctionParams(process(afp.sig), processCB(afp.body))

  //  def process(at: AnyType)(implicit state: IRState): AnyType = ???
  //  def process(nt: NumberType)(implicit state: IRState): NumberType = ???
  //  def process(bt: BooleanType)(implicit state: IRState): BooleanType = ???
  //  def process(st: StringType)(implicit state: IRState): StringType = ???
  //  def process(st: SymbolType)(implicit state: IRState): SymbolType = ???
  //  def process(vt: VoidType)(implicit state: IRState): VoidType = ???
  //  def process(tt: ThisType)(implicit state: IRState): ThisType = ???
  def process(ut: UnionType)(implicit state: IRState): UnionType = UnionType(processT(ut.t1), processT(ut.t2))

  def process(it: IntersectionType)(implicit state: IRState): IntersectionType = IntersectionType(processT(it.t1), processT(it.t2))

  def process(pt: ParenthesisedType)(implicit state: IRState): ParenthesisedType = ParenthesisedType(processT(pt.t))

  def process(at: ArrayType)(implicit state: IRState): ArrayType = ArrayType(processT(at.t))

  def process(tt: TupleType)(implicit state: IRState): TupleType = TupleType(tt.ts.map(processT))

  def process(tp: TypeParameters)(implicit state: IRState): TypeParameters = TypeParameters(tp.params.map(process))

  def process(tp: TypeParameter)(implicit state: IRState): TypeParameter = TypeParameter(process(tp.id), tp.constraint.map(processT))

  def process(ta: TypeArguments)(implicit state: IRState): TypeArguments = TypeArguments(ta.ts.map(processT))

  def process(tr: TypeReference)(implicit state: IRState): TypeReference = TypeReference(tr.refs.map(processE), tr.args.map(process))

  def process(tq: TypeQuery)(implicit state: IRState): TypeQuery = TypeQuery(tq.refs.map(processE))

  def process(riop: RequiredIdentifierOrPattern)(implicit state: IRState): RequiredIdentifierOrPattern = RequiredIdentifierOrPattern(riop.modifier, processBPatt(riop.bind), riop.annotation.map(processT))

  def process(riws: RequiredIdentifierWithString)(implicit state: IRState): RequiredIdentifierWithString = RequiredIdentifierWithString(process(riws.id), process(riws.s))

  def process(oiop: OptionalIdentifierOrPattern)(implicit state: IRState): OptionalIdentifierOrPattern = OptionalIdentifierOrPattern(oiop.modifier, processBPatt(oiop.bind), oiop.annotation.map(processT))

  def process(oiopi: OptionalIdentifierOrPatternInit)(implicit state: IRState): OptionalIdentifierOrPatternInit = OptionalIdentifierOrPatternInit(oiopi.modifier, processBPatt(oiopi.bind), oiopi.annotation.map(processT), processE(oiopi.e))

  def process(oiws: OptionalIdentifierWithString)(implicit state: IRState): OptionalIdentifierWithString = OptionalIdentifierWithString(process(oiws.id), process(oiws.s))

  def process(rsp: RestSpreadParameter)(implicit state: IRState): RestSpreadParameter = RestSpreadParameter(process(rsp.id), rsp.annotation.map(processT))

  def process(pl: ParameterList)(implicit state: IRState): ParameterList = ParameterList(pl.contents.map(processPLI))

  def process(ps: PropertySignature)(implicit state: IRState): PropertySignature = PropertySignature(processE(ps.name), ps.q, ps.annotation.map(processT))

  def process(cs: CallSignature)(implicit state: IRState): CallSignature = CallSignature(cs.tParams.map(process), cs.params.map(process), cs.annotation.map(processT))

  def process(cs: ConstructSignature)(implicit state: IRState): ConstructSignature = ConstructSignature(cs.tParams.map(process), cs.params.map(process), cs.annotation.map(processT))

  def process(is: IndexSignature)(implicit state: IRState): IndexSignature = IndexSignature(process(is.id), is.t, processT(is.annotation))

  def process(ms: MethodSignature)(implicit state: IRState): MethodSignature = MethodSignature(processE(ms.name), ms.q, process(ms.sig))

  def process(ot: ObjectType)(implicit state: IRState): ObjectType = ObjectType(ot.body.map(processSig))

  def process(tad: TypeAliasDeclaration)(implicit state: IRState): TypeAliasDeclaration = TypeAliasDeclaration(process(tad.id), tad.params.map(process), processT(tad.t))

  def process(a: Arguments)(implicit state: IRState): Arguments = Arguments(a.typeArgs.map(process), a.args.map(processE))

  def process(ed: EnumDeclaration)(implicit state: IRState): EnumDeclaration = EnumDeclaration(ed.const, process(ed.id), ed.members.map(process))

  def process(em: EnumMember)(implicit state: IRState): EnumMember = EnumMember(processE(em.name), em.e.map(processE))

  def process(ft: FunctionType)(implicit state: IRState): FunctionType = FunctionType(ft.tParam.map(process), ft.params.map(process), processT(ft.result))

  def process(id: InterfaceDeclaration)(implicit state: IRState): InterfaceDeclaration = InterfaceDeclaration(process(id.id), id.tParam.map(process), id.ext.map(_.map(process)), process(id.oT))

  def process(cd: ClassDeclaration)(implicit state: IRState): ClassDeclaration = ClassDeclaration(cd.id.map(process), cd.tParam.map(process), cd.ext.map(process), cd.imp.map(_.map(process)), cd.body.map(processCE))

  def process(cd: ConstructorDeclaration)(implicit state: IRState): ConstructorDeclaration = ConstructorDeclaration(cd.modifier, cd.params.map(process), cd.body.map(process))

  def process(imd: IndexMemberDeclaration)(implicit state: IRState): IndexMemberDeclaration = IndexMemberDeclaration(process(imd.is))

  def process(mvd: MemberVariableDeclaration)(implicit state: IRState): MemberVariableDeclaration = MemberVariableDeclaration(mvd.modifier, mvd.static, processE(mvd.name), mvd.annotation.map(processT), mvd.init.map(processE))

  def process(mfd: MemberFunctionDeclaration)(implicit state: IRState): MemberFunctionDeclaration = MemberFunctionDeclaration(mfd.modifier, processE(mfd.name), process(mfd.call), mfd.body.map(process))

  def processE(e: Expression)(implicit state: IRState): Expression = e match {
    case ir: IR => processIR(ir)
    case af: ArrowFunction => processAF(af)
    case l: Literal => process(l)
    case i: Identifier => process(i)
    case al: ArrayLiteral => process(al)
    case ol: ObjectLiteral => process(ol)
    case s: Spread => process(s)
    case pe: PostfixExpr => process(pe)
    case kpe: KeywordPrefixExpr => process(kpe)
    case pe: PrefixExpr => process(pe)
    case tce: TypeCastExpr => process(tce)
    case boe: BinaryOpExpr => process(boe)
    case ife: InfixFuncExpr => process(ife)
    case te: TernaryExpr => process(te)
    case mde: MemberDotExpr => process(mde)
    case mie: MemberIndexExpr => process(mie)
    case pe: ParenthesisedExpr => process(pe)
    case nme: NewMemberExpr => process(nme)
    //    case ASTNodes.Elision =>
    //    case ASTNodes.This =>
    //    case ASTNodes.Super =>
    //    case ASTNodes.Yield =>
    case cpn: ComputedPropertyName => process(cpn)
    case mc: MemberCall => process(mc)
    case sc: SuperCall => process(sc)
    case nc: NestedCall => process(nc)
    case ic: IndexedCall => process(ic)
    case dc: DotCall => process(dc)
    case im: IndexedMember => process(im)
    case dm: DotMember => process(dm)
    //    case ASTNodes.MetaProperty =>
    case spd: SuperPropertyDot => process(spd)
    case spi: SuperPropertyIndexed => process(spi)
    case fe: FunctionExpression => process(fe)
    case _ => e
  }

  def processPD(pd: PropertyDefinition)(implicit state: IRState): PropertyDefinition = pd match {
    case i: Id => process(i)
    case i: Identifier => process(i)
    case pea: PropertyExpressionAssignment => process(pea)
    case cin: CoverInitialisedName => process(cin)
    case mp: MethodProperty => process(mp)
  }

  def processMI(mi: ModuleItem)(implicit state: IRState): ModuleItem = mi match {
    case id: ImportDeclaration => processID(id)
    case sli: StatementListItem => processSLI(sli)
    case ed: ExportDeclaration => processED(ed)
  }

  def processID(id: ImportDeclaration)(implicit state: IRState): ImportDeclaration = id match {
    case ims: ImportModuleSpecifier => process(ims)
    case ifrom: ImportFrom => process(ifrom)
    case ir: ImportRequire => process(ir)
  }

  def processIC(ic: ImportClause)(implicit state: IRState): ImportClause = ic match {
    case ibc: ImportBindingClause => process(ibc)
    case nsic: NameSpaceImportClause => process(nsic)
    case nic: NamedImportsClause => process(nic)
    case bfc: BindingFollowedClause => process(bfc)
  }

  def processStat(s: Statement)(implicit state: IRState): Statement = s match {
    case es: ExpressionSequence => process(es)
    //    case ASTNodes.EmptyStatement =>
    //    case ASTNodes.DebuggerStatement =>
    case vs: VariableStatement => process(vs)
    case lvs: LazyVariableStatement => process(lvs)
    case ivs: InlineVariableStatement => process(ivs)
    case ifs: InlineFunctionStatement => process(ifs)
    case bs: BlockStatement => process(bs)
    case is: IfStatement => process(is)
    case ds: DoStatement => process(ds)
    case ws: WhileStatement => process(ws)
    case fs: ForStatement => process(fs)
    case fvs: ForVarStatement => process(fvs)
    case fis: ForInStatement => process(fis)
    case fvs: ForVarInStatement => process(fvs)
    case cs: ContinueStatement => process(cs)
    case bs: BreakStatement => process(bs)
    case rs: ReturnStatement => process(rs)
    case ts: ThrowStatement => process(ts)
    case ws: WithStatement => process(ws)
    case ss: SwitchStatement => process(ss)
    case ls: LabelledStatement => process(ls)
    case tcs: TryCatchStatement => process(tcs)
    case tfs: TryFinallyStatement => process(tfs)
    case _ => s
  }

  def processSLI(sli: StatementListItem)(implicit state: IRState): StatementListItem = sli match {
    case s: Statement => processStat(s)
    case d: Declaration => processD(d)
  }

  def processED(ed: ExportDeclaration)(implicit state: IRState): ExportDeclaration = ed match {
    case es: ExportStar => process(es)
    case ewc: ExportWithClause => process(ewc)
    case ev: ExportVar => process(ev)
    case ef: ExportFun => process(ef)
  }

  def processBPatt(bp: BindingPattern)(implicit state: IRState): BindingPattern = bp match {
    case i: Id => process(i)
    case i: Identifier => process(i)
    case bo: BindingObject => process(bo)
    case ba: BindingArray => process(ba)
  }

  def processSC(sc: SwitchClause)(implicit state: IRState): SwitchClause = sc match {
    case cc: CaseClause => process(cc)
    case dc: DefaultClause => process(dc)
  }

  def processAF(af: ArrowFunction)(implicit state: IRState): ArrowFunction = af match {
    case afi: ArrowFunctionIdentifier => process(afi)
    case afp: ArrowFunctionParams => process(afp)
  }

  def processCB(cb: ConciseBody)(implicit state: IRState): ConciseBody = cb match {
    case e: Expression => processE(e)
    case fb: FunctionBody => process(fb)
  }

  def processT(t: Type)(implicit state: IRState): Type = t match {
    case l: Literal => process(l)
    //    case ASTNodes.AnyType =>
    //    case ASTNodes.NumberType =>
    //    case ASTNodes.BooleanType =>
    //    case ASTNodes.StringType =>
    //    case ASTNodes.SymbolType =>
    //    case ASTNodes.VoidType =>
    //    case ASTNodes.ThisType =>
    case ut: UnionType => process(ut)
    case it: IntersectionType => process(it)
    case pt: ParenthesisedType => process(pt)
    case at: ArrayType => process(at)
    case tt: TupleType => process(tt)
    case tr: TypeReference => process(tr)
    case tq: TypeQuery => process(tq)
    case ot: ObjectType => process(ot)
    case ft: FunctionType => process(ft)
    case _ => t
  }

  def processBProp(bp: BindingProperty)(implicit state: IRState): BindingProperty = bp match {
    case snb: SingleNameBinding => process(snb)
    case bpn: BindingPropertyName => process(bpn)
  }

  def processBE(be: BindingElement)(implicit state: IRState): BindingElement = be match {
    //    case ASTNodes.BindingElision =>
    case snb: SingleNameBinding => process(snb)
    case bpe: BindingPatternElement => process(bpe)
    case bs: BindingSpread => process(bs)
    case _ => be
  }

  def processD(d: Declaration)(implicit state: IRState): Declaration = d match {
    case fd: FunctionDeclaration => processFD(fd)
    case tad: TypeAliasDeclaration => process(tad)
    case ed: EnumDeclaration => process(ed)
    case id: InterfaceDeclaration => process(id)
    case cd: ClassDeclaration => process(cd)
  }

  def processFD(fd: FunctionDeclaration)(implicit state: IRState): FunctionDeclaration = fd match {
    case nefd: NonEmptyFunctionDeclaration => process(nefd)
    case efd: EmptyFunctionDeclaration => process(efd)
  }

  def processSig(s: Signature)(implicit state: IRState): Signature = s match {
    case ps: PropertySignature => process(ps)
    case cs: CallSignature => process(cs)
    case cs: ConstructSignature => process(cs)
    case is: IndexSignature => process(is)
    case ms: MethodSignature => process(ms)
  }

  def processPLI(pli: ParameterListItem)(implicit state: IRState): ParameterListItem = pli match {
    case riop: RequiredIdentifierOrPattern => process(riop)
    case riws: RequiredIdentifierWithString => process(riws)
    case oiop: OptionalIdentifierOrPattern => process(oiop)
    case oiopi: OptionalIdentifierOrPatternInit => process(oiopi)
    case oiws: OptionalIdentifierWithString => process(oiws)
    case rsp: RestSpreadParameter => process(rsp)
  }

  def processCE(ce: ClassElement)(implicit state: IRState): ClassElement = ce match {
    case cd: ConstructorDeclaration => process(cd)
    case imd: IndexMemberDeclaration => process(imd)
    case pmd: PropertyMemberDeclaration => processPMD(pmd)
  }

  def processPMD(pmd: PropertyMemberDeclaration)(implicit state: IRState): PropertyMemberDeclaration = pmd match {
    case mvd: MemberVariableDeclaration => process(mvd)
    case mfd: MemberFunctionDeclaration => process(mfd)
  }

  // IR traversal

  def process(irl: IRLiteral)(implicit state: IRState): IRLiteral = IRLiteral(process(irl.l))

  def process(i: Id)(implicit state: IRState): Id = i

  def process(a: Atom)(implicit state: IRState): Atom = a

  def process(a: Ap)(implicit state: IRState): Ap = Ap(processIR(a.l), processIR(a.r))

  def process(p: Pa)(implicit state: IRState): Pa = Pa(processIR(p.l), processIR(p.r))

  def process(al: ApL)(implicit state: IRState): ApL = ApL(processIR(al.l), processIR(al.r))

  def process(ar: ApR)(implicit state: IRState): ApR = ApR(processIR(ar.l), processIR(ar.r))

  def process(c: Choice)(implicit state: IRState): Choice = Choice(processIR(c.l), processIR(c.r))

  def process(m: Mult)(implicit state: IRState): Mult = Mult(processIR(m.l), processIR(m.r))

  def process(ml: MultL)(implicit state: IRState): MultL = MultL(processIR(ml.l), processIR(ml.r))

  def process(mr: MultR)(implicit state: IRState): MultR = MultR(processIR(mr.l), processIR(mr.r))

  def process(f: Fmap)(implicit state: IRState): Fmap = Fmap(processIR(f.l), processIR(f.r))

  def process(p: Pamf)(implicit state: IRState): Pamf = Pamf(processIR(p.l), processIR(p.r))

  def process(cfl: ConstFmapL)(implicit state: IRState): ConstFmapL = ConstFmapL(processIR(cfl.l), processIR(cfl.r))

  def process(cfr: ConstFmapR)(implicit state: IRState): ConstFmapR = ConstFmapR(processIR(cfr.l), processIR(cfr.r))

  def process(lc: LiftCons)(implicit state: IRState): LiftCons = LiftCons(processIR(lc.l), processIR(lc.r))

  def process(l: Label)(implicit state: IRState): Label = Label(processIR(l.l), processIR(l.r))

  def process(p: Pure)(implicit state: IRState): Pure = Pure(p.typeArgs.map(process), p.args.map(processIR))

  def process(s: Satisfy)(implicit state: IRState): Satisfy = Satisfy(s.typeArgs.map(process), s.args.map(processIR))

  def process(c: Chr)(implicit state: IRState): Chr = Chr(c.typeArgs.map(process), c.args.map(processIR))

  def process(s: Str)(implicit state: IRState): Str = Str(s.typeArgs.map(process), s.args.map(processIR))

  def process(m1: Many1)(implicit state: IRState): Many1 = Many1(m1.typeArgs.map(process), m1.args.map(processIR))

  def process(m: Many)(implicit state: IRState): Many = Many(m.typeArgs.map(process), m.args.map(processIR))

  def process(a: Attempt)(implicit state: IRState): Attempt = Attempt(a.typeArgs.map(process), a.args.map(processIR))

  def process(t: Token)(implicit state: IRState): Token = Token(t.typeArgs.map(process), t.args.map(processIR))

  def process(ct: CT)(implicit state: IRState): CT = CT(ct.typeArgs.map(process), ct.args.map(processIR))

  def process(cl1: ChainL1)(implicit state: IRState): ChainL1 = ChainL1(cl1.typeArgs.map(process), cl1.args.map(processIR))

  def process(cr1: ChainR1)(implicit state: IRState): ChainR1 = ChainR1(cr1.typeArgs.map(process), cr1.args.map(processIR))

  def process(l2: Lift2)(implicit state: IRState): Lift2 = Lift2(l2.typeArgs.map(process), l2.args.map(processIR))

  def process(l3: Lift3)(implicit state: IRState): Lift3 = Lift3(l3.typeArgs.map(process), l3.args.map(processIR))

  def process(l4: Lift4)(implicit state: IRState): Lift4 = Lift4(l4.typeArgs.map(process), l4.args.map(processIR))

  def process(l5: Lift5)(implicit state: IRState): Lift5 = Lift5(l5.typeArgs.map(process), l5.args.map(processIR))

  def process(l6: Lift6)(implicit state: IRState): Lift6 = Lift6(l6.typeArgs.map(process), l6.args.map(processIR))

  def process(l7: Lift7)(implicit state: IRState): Lift7 = Lift7(l7.typeArgs.map(process), l7.args.map(processIR))

  def process(l8: Lift8)(implicit state: IRState): Lift8 = Lift8(l8.typeArgs.map(process), l8.args.map(processIR))

  def process(l9: Lift9)(implicit state: IRState): Lift9 = Lift9(l9.typeArgs.map(process), l9.args.map(processIR))

  def process(p: Postfix)(implicit state: IRState): Postfix = Postfix(p.typeArgs.map(process), p.args.map(processIR))

  def process(e: Empty)(implicit state: IRState): Empty = Empty(e.typeArgs.map(process), e.args.map(processIR))

  def process(b: Between)(implicit state: IRState): Between = Between(b.typeArgs.map(process), b.args.map(processIR))

  def process(r: Re)(implicit state: IRState): Re = Re(r.typeArgs.map(process), r.args.map(processIR))

  def process(c: Curry)(implicit state: IRState): Curry = Curry(processIR(c.f))

  def process(f: Flip)(implicit state: IRState): Flip = Flip(processIR(f.f))

  def process(c: Const)(implicit state: IRState): Const = Const(processIR(c.c))

  def process(mfa: MapFirstArg)(implicit state: IRState): MapFirstArg = MapFirstArg(processIR(mfa.g), processIR(mfa.f))

  def process(u: Uncurry)(implicit state: IRState): Uncurry = Uncurry(processIR(u.f))

  def process(ufa: UnpairFirstArg)(implicit state: IRState): UnpairFirstArg = UnpairFirstArg(processIR(ufa.f))

  def process(pt: PaTransform)(implicit state: IRState): PaTransform = PaTransform(processIR(pt.f))

  def process(att: ArgsToTuple)(implicit state: IRState): ArgsToTuple = ArgsToTuple(processIR(att.f))

  def process(lsc: LiftStringConcat)(implicit state: IRState): LiftStringConcat = LiftStringConcat(processIR(lsc.p), processIR(lsc.q))

  def process(e: Explicit)(implicit state: IRState): Explicit = Explicit(processE(e.e))

  def process(lc: LeadingChar)(implicit state: IRState): LeadingChar = lc

  def processC(c: Combinator)(implicit state: IRState): Combinator = c match {
    case a: Ap => process(a)
    case p: Pa => process(p)
    case al: ApL => process(al)
    case ar: ApR => process(ar)
    case c: Choice => process(c)
    case m: Mult => process(m)
    case ml: MultL => process(ml)
    case mr: MultR => process(mr)
    case f: Fmap => process(f)
    case p: Pamf => process(p)
    case cfl: ConstFmapL => process(cfl)
    case cfr: ConstFmapR => process(cfr)
    case lc: LiftCons => process(lc)
    case l: Label => process(l)
  }

  def processF(f: Func)(implicit state: IRState): Func = f match {
    case p: Pure => process(p)
    case s: Satisfy => process(s)
    case c: Chr => process(c)
    case s: Str => process(s)
    case m1: Many1 => process(m1)
    case m: Many => process(m)
    case a: Attempt => process(a)
    case t: Token => process(t)
    case ct: CT => process(ct)
    case cl1: ChainL1 => process(cl1)
    case cr1: ChainR1 => process(cr1)
    case l2: Lift2 => process(l2)
    case l3: Lift3 => process(l3)
    case l4: Lift4 => process(l4)
    case l5: Lift5 => process(l5)
    case l6: Lift6 => process(l6)
    case l7: Lift7 => process(l7)
    case l8: Lift8 => process(l8)
    case l9: Lift9 => process(l9)
    case p: Postfix => process(p)
    case e: Empty => process(e)
    case b: Between => process(b)
    case r: Re => process(r)
  }

  def processG(g: Generated)(implicit state: IRState): Generated = g match {
    case s: Special => processSpec(s)
    case gf: GeneratedFunc => processGF(gf)
  }

  def processGF(gf: GeneratedFunc)(implicit state: IRState): GeneratedFunc = gf match {
    case c: Curry => process(c)
    case f: Flip => process(f)
    case c: Const => process(c)
    case mfa: MapFirstArg => process(mfa)
    case u: Uncurry => process(u)
    case ufa: UnpairFirstArg => process(ufa)
    case pt: PaTransform => process(pt)
    case att: ArgsToTuple => process(att)
    case lsc: LiftStringConcat => process(lsc)
  }

  def processSpec(s: Special)(implicit state: IRState): Special = s match {
    case e: Explicit => process(e)
    case lc: LeadingChar => process(lc)
    case _ => s
  }

  def processIR(ir: IR)(implicit state: IRState): IR = ir match {
    case irl: IRLiteral => process(irl)
    case a: Atom => process(a)
    case i: Id => process(i)
    case c: Combinator => processC(c)
    case f: Func => processF(f)
    case g: Generated => processG(g)
  }
}
