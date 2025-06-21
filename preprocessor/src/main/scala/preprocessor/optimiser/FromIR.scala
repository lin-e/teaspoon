package preprocessor.optimiser

import preprocessor.optimiser.IRNodes._
import preprocessor.parser.ASTNodes._

object FromIR extends PipelineStage {

  override def processE(e: Expression)(implicit state: IRState): Expression = e match {
    case ir: IR => ir match {
      case IRLiteral(l) => process(l)
      case a: Atom => processE(state.getExpr(a))
      case i: Id => identifierFromIR(i)
      case combinator: Combinator =>
        val le = processE(combinator.l)
        val re = processE(combinator.r)

        combinator match {
          case _: Ap => BinaryOpExpr(le, re, "<*>")
          case _: Pa => BinaryOpExpr(le, re, "<**>")
          case _: ApL => BinaryOpExpr(le, re, "<*")
          case _: ApR => BinaryOpExpr(le, re, "*>")
          case _: Choice => BinaryOpExpr(le, re, "<|>")
          case _: Mult => BinaryOpExpr(le, re, "<~>")
          case _: MultL => BinaryOpExpr(le, re, "<~")
          case _: MultR => BinaryOpExpr(le, re, "~>")
          case _: Fmap => BinaryOpExpr(le, re, "<$>")
          case _: Pamf => BinaryOpExpr(le, re, "<&>")
          case _: ConstFmapL => BinaryOpExpr(le, re, "<$")
          case _: ConstFmapR => BinaryOpExpr(le, re, "$>")
          case _: LiftCons => BinaryOpExpr(le, re, "<:>")
          case _: Label => BinaryOpExpr(le, re, "<?>")
        }
      case func: Func =>
        def memberCallAndAdd(f: String): MemberCall = {
          val args = Arguments(func.typeArgs, func.args.map(processE))
          state.addImport(f)
          MemberCall(Identifier(f), args)
        }

        func match {
          case _: Pure => memberCallAndAdd("pure")
          case _: Satisfy => memberCallAndAdd("satisfy")
          case _: Chr => memberCallAndAdd("chr")
          case _: Str => memberCallAndAdd("str")
          case _: Many1 => memberCallAndAdd("many1")
          case _: Many => memberCallAndAdd("many")
          case _: Attempt => memberCallAndAdd("attempt")
          case _: Token => memberCallAndAdd("token")
          case _: CT => memberCallAndAdd("cT")
          case _: ChainL1 => memberCallAndAdd("chainl1")
          case _: ChainR1 => memberCallAndAdd("chainr1")
          case _: Lift2 => memberCallAndAdd("lift2")
          case _: Lift3 => memberCallAndAdd("lift3")
          case _: Lift4 => memberCallAndAdd("lift4")
          case _: Lift5 => memberCallAndAdd("lift5")
          case _: Lift6 => memberCallAndAdd("lift6")
          case _: Lift7 => memberCallAndAdd("lift7")
          case _: Lift8 => memberCallAndAdd("lift8")
          case _: Lift9 => memberCallAndAdd("lift9")
          case _: Postfix => memberCallAndAdd("postfix")
          case _: Empty => memberCallAndAdd("empty")
          case _: Between => memberCallAndAdd("between")
          case _: Re => memberCallAndAdd("re")
        }
      case g: Generated => g match {
        case gf: GeneratedFunc =>
          def genCall(name: String, as: IR*): MemberCall = {
            state.addImport(name)
            MemberCall(Identifier(name), Arguments(None, as.toList.map(processE)))
          }

          gf match {
            case Curry(Uncurry(f)) => processE(f)
            case Uncurry(Curry(f)) => processE(f)
            case Flip(Flip(f)) => processE(f)

            case Curry(f) => processE(f) match {
              case TSFunction2(x, y, f) => TSFunction1(x, TSFunction1(y, f))
              case _ => genCall("curry", f)
            }
            case Flip(f) => processE(f) match {
              case TSFunction1(x, TSFunction1(y, f)) => TSFunction1(y, TSFunction1(x, f))
              case _ => genCall("flip", f)
            }
            case Const(c) => ArrowFunctionIdentifier(Identifier("_"), processE(c))
            case MapFirstArg(g, f) => processE(f) match {
              case ff@TSFunction2(IdentifierParam(xi, _), y@IdentifierParam(yi, _), _) =>
                val gx = MemberCall(processE(g), Arguments(None, List(xi)))
                TSFunction2(IdentifierParam(xi, None), y, MemberCall(ff, Arguments(None, List(gx, yi))))
              case _ => genCall("mapFirstArg", g, f)
            }
            case Uncurry(f) => processE(f) match {
              case TSFunction1(x, TSFunction1(y, f)) => TSFunction2(x, y, f)
              case _ => genCall("uncurry", f)
            }
            case UnpairFirstArg(f) => processE(f) match {
              //                case TSFunction2(PairIdentifierParam(i, j, ts), IdentifierParam(k, t), f) => (ts, t) match {
              //                  case (Some((t1, _)), None) => TSFunction2(IdentifierParam(i, Some(t1)), PairIdentifierParam(j, k, None), f)
              //                  case (None, _) => TSFunction2(IdentifierParam(i, None), PairIdentifierParam(j, k, None), f)
              //                  case (Some((t1, t2)), Some(t)) => TSFunction2(IdentifierParam(i, Some(t1)), PairIdentifierParam(j, k, Some((t2, t))), f)
              //                }
              case TSFunction2(PairUnknownParam(p1, p2, ts), IdentifierParam(p3, t), f) =>
                val (nt1, nt2) = (ts, t) match {
                  case (Some(P(t1, _)), None) => (Some(t1), None)
                  case (Some(P(t1, t2)), Some(t)) => (Some(t1), Some(P(t2, I(t))))
                  case _ => (None, None)
                }
                TSFunction2(btToPLI(p1, nt1), PairUnknownParam(p2, I(p3), nt2), f)
              case TSFunction2(PairUnknownParam(p1, p2, ts1), PairUnknownParam(p3, p4, ts2), f) =>
                val (nt1, nt2) = (ts1, ts2) match {
                  case (Some(P(t1, _)), None) => (Some(t1), None)
                  case (Some(P(t1, t2)), Some(t)) => (Some(t1), Some(P(t2, t)))
                  case _ => (None, None)
                }
                TSFunction2(btToPLI(p1, nt1), PairUnknownParam(p2, P(p3, p4), nt2), f)
              case _ => genCall("unpairFirstArg", f)
            }
            case PaTransform(f) => genCall("paTransform", f)
            case ArgsToTuple(f) => processE(f) match {
              case TSFunction1(PairUnknownParam(p1, p2, ts), f) => ts match {
                case Some(P(t1, t2)) => TSFunction2(btToPLI(p1, Some(t1)), btToPLI(p2, Some(t2)), f)
                case _ => TSFunction2(btToPLI(p1, None), btToPLI(p2, None), f)
              }
              case _ => genCall("argsToTuple", f)
            }
            case LiftStringConcat(p, q) => genCall("lsc", p, q)
          }
        case s: Special =>
          def identifierAndAdd(id: String): Identifier = {
            state.addImport(id)
            Identifier(id)
          }

          s match {
            case Explicit(e) => e
            case ConstFunction => ArrowFunctionIdentifier(Identifier("c"), ArrowFunctionIdentifier(Identifier("_"), Identifier("c")))
            case ApFromMult => identifierAndAdd("apFromMult")
            case PaFromMult => identifierAndAdd("paFromMult")
            case StringEpsilon => identifierAndAdd("se")
            case LeadingChar(_) => ??? // bug if it reaches this case
            case Cons => identifierAndAdd("cons")
          }
      }
    }
    case _ => super.processE(e)
  }

  override def processPD(pd: PropertyDefinition)(implicit state: IRState): PropertyDefinition = pd match {
    case i: Id => identifierFromIR(i)
    case _ => super.processPD(pd)
  }

  override def processBPatt(bp: BindingPattern)(implicit state: IRState): BindingPattern = bp match {
    case i: Id => identifierFromIR(i)
    case _ => super.processBPatt(bp)
  }


  sealed trait ParamBT[A]

  case class P[A](a: ParamBT[A], b: ParamBT[A]) extends ParamBT[A]

  case class I[A](i: A) extends ParamBT[A]

  type ParamBTI = ParamBT[Identifier]
  type ParamBTT = ParamBT[Type]

  private object PairUnknownParam {
    def unapply(p: ParameterListItem): Option[(ParamBTI, ParamBTI, Option[ParamBTT])] = p match {
      case RequiredIdentifierOrPattern(_, BindingArray(e1 :: BindingElision :: e2 :: Nil), t) => for {
        p1 <- elemToBT(e1)
        p2 <- elemToBT(e2)
      } yield (p1, p2, t.flatMap(typeToParam))
      case _ => None
    }
    def apply(p1: ParamBTI, p2: ParamBTI, t: Option[ParamBTT]): ParameterListItem = {
      val ba = BindingArray(List(btToElem(p1), BindingElision, btToElem(p2)))
      val tt = t.map(btToType)

      RequiredIdentifierOrPattern(None, ba, tt)
    }
  }

  private object PairElem {
    def unapply(b: BindingElement): Option[P[Identifier]] = b match {
      case BindingPatternElement(BindingArray(e1 :: BindingElision :: e2 :: Nil), None) => for {
        a <- elemToBT(e1)
        b <- elemToBT(e2)
      } yield P(a, b)

      case _ => None
    }
    def apply(p: P[Identifier]): BindingElement = p match {
      case P(a, b) => BindingPatternElement(BindingArray(List(btToElem(a), BindingElision, btToElem(b))), None)
    }
  }

  private object PairType {
    def unapply(t: Type): Option[P[Type]] = t match {
      case TupleType(t1 :: t2 :: Nil) => for {
        a <- typeToParam(t1)
        b <- typeToParam(t2)
      } yield P(a, b)

      case _ => None
    }
    def apply(p: P[Type]): Type = p match {
      case P(a, b) => TupleType(List(btToType(a), btToType(b)))
    }
  }

  private object IdElem {
    def unapply(b: BindingElement): Option[I[Identifier]] = {
      b match {
        case SingleNameBinding(id: Identifier, None) => Some(I(id))
        case _ => None
      }
    }
    def apply(i: I[Identifier]): BindingElement = SingleNameBinding(i.i, None)
  }

  private object IdType {
    def unapply(t: Type): Option[I[Type]] = Some(I(t)) // will always succeed; should be last case
    def apply(i: I[Type]): Type = i.i
  }

  private object IdentifierParam {
    def unapply(p: ParameterListItem): Option[(Identifier, Option[Type])] = p match {
      case RequiredIdentifierOrPattern(_, i: Identifier, t) => Some((i, t))
      case _ => None
    }
    def apply(i: Identifier, t: Option[Type]): ParameterListItem = RequiredIdentifierOrPattern(None, i, t)
  }

  private object PairIdentifierParam {
    def unapply(p: ParameterListItem): Option[(Identifier, Identifier, Option[(Type, Type)])] = p match {
      case RequiredIdentifierOrPattern(_, BindingArray(List(SingleNameBinding(i: Identifier, _), BindingElision, SingleNameBinding(j: Identifier, _))), t) => t match {
        case Some(TupleType(t1 :: t2 :: Nil)) => Some((i, j, Some((t1, t2))))
        case _ => Some((i, j, None))
      }
      case _ => None
    }
    def apply(i: Identifier, j: Identifier, ts: Option[(Type, Type)]): ParameterListItem = {
      val ba = BindingArray(List(SingleNameBinding(i, None), BindingElision, SingleNameBinding(j, None)))
      val t = ts.map { case (t1, t2) => TupleType(List(t1, t2)) }

      RequiredIdentifierOrPattern(None, ba, t)
    }
  }

  private object TSFunction1 {
    def unapply(e: Expression): Option[(ParameterListItem, ConciseBody)] = e match {
      case ArrowFunctionParams(CallSignature(None, Some(ParameterList(x :: Nil)), None), f) => Some((x, f))
      case ArrowFunctionIdentifier(id, f) => Some((RequiredIdentifierOrPattern(None, id, None), f))
//      case ArrowFunctionParams(CallSignature(None, Some(ParameterList(x :: Nil)), None), f) => Some((x, f))
      case _ => None
    }
    def apply(x: ParameterListItem, f: ConciseBody): Expression =
      ArrowFunctionParams(CallSignature(None, Some(ParameterList(x :: Nil)), None), f)
  }

  private object TSFunction2 {
    def unapply(e: Expression): Option[(ParameterListItem, ParameterListItem, ConciseBody)] = e match {
      case ArrowFunctionParams(CallSignature(None, Some(ParameterList(x :: y :: Nil)), None), f) => Some((x, y, f))
      case _ => None
    }
    def apply(x: ParameterListItem, y: ParameterListItem, f: ConciseBody): Expression =
      ArrowFunctionParams(CallSignature(None, Some(ParameterList(x :: y :: Nil)), None), f)
  }

  private def elemToBT(b: BindingElement): Option[ParamBTI] = b match {
    case PairElem(p) => Some(p)
    case IdElem(i) => Some(i)
    case _ => None
  }

  private def btToElem(p: ParamBTI): BindingElement = p match {
    case pp@P(_, _) => PairElem(pp)
    case ii@I(_) => IdElem(ii)
  }

  private def btToPLI(p: ParamBTI, t: Option[ParamBTT]): ParameterListItem = {
    val bind = btToElem(p) match {
      case SingleNameBinding(id, _) => id
      case BindingPatternElement(ba: BindingArray, _) => ba
      case _ => ???
    }

    RequiredIdentifierOrPattern(None, bind, t.map(btToType))
  }

  private def typeToParam(t: Type): Option[ParamBTT] = t match {
    case PairType(p) => Some(p)
    case IdType(i) => Some(i)
    case _ => None
  }

  private def btToType(p: ParamBTT): Type = p match {
    case p@P(_, _) => PairType(p)
    case i@I(_) => IdType(i)
  }

  private def identifierFromIR(i: Id)(implicit state: IRState): Identifier = Identifier(i.name)
}
