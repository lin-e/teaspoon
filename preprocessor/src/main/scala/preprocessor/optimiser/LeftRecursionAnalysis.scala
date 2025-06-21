package preprocessor.optimiser

import preprocessor.optimiser.IRNodes._
import preprocessor.parser.ASTNodes._
import Utils.{c2l => _, _}

object LeftRecursionAnalysis extends PipelineStage {
  type RewriteResult = Either[IR, IR]

  override def processStat(s: Statement)(implicit state: IRState): Statement = s match {
    case LazyVariableStatement(lhs, annotation, rhs) =>
      val (recursive, atoms) = c2l(rhs).partition(simpleLeftRecursive(lhs, _))
      val pLhs = processBPatt(lhs)

      // global check
      val globalLeftRecursiveAtoms = atoms.filter { globalLeftRecursive(lhs, _) }
      if (globalLeftRecursiveAtoms.nonEmpty) {
        state.addMessage(f"LeftRecursionAnalysis: Some atoms for '${printIR(lhs)}' may have non-local left-recursion:")
        globalLeftRecursiveAtoms.foreach(addStateMessage)
      }

      def wrapAttempt(ir: IR): IR = Attempt(None, List(ir))
      def wrapAndReduce(irs: List[IR]): IR = irs match {
        case ir :: Nil => ir
        case Nil => ???
        case _ => irs.map(wrapAttempt).reduce(Choice)
      }

      val newRhs = (recursive, atoms) match {
        case (_, Nil) =>
          state.addMessage(f"LeftRecursionAnalysis: No atoms (non-left-recursive) found for '${printIR(lhs)}'")
          rhs
        case (Nil, _) => rhs // no left recursion
        case _ =>
          val atom = wrapAndReduce(atoms.map(stripAttempt)) // Assume everything is attempt
          val recs: List[RewriteResult] = recursive
            .map(stripAttempt) // Assume everything is attempt
            .flatMap {
              case l: Lift2 => List(rewriteLift2(l, lhs))
              case Ap(Ap(_Pure(_, f), q), r) => List(rewriteLift2(_Lift2(None, Uncurry(f), q, r), lhs))
              case Ap(Fmap(f, q), r) => List(rewriteLift2(_Lift2(None, Uncurry(f), q, r), lhs)) // f <$> q <*> r ### same shape as <:>
              case Ap(Pamf(q, f), r) => List(rewriteLift2(_Lift2(None, Uncurry(f), q, r), lhs)) // q <&> f <*> r
              case Pamf(Mult(q, r), f) => List(rewriteLift2(_Lift2(None, ArgsToTuple(f), q, r), lhs)) // q <~> r <&> f
              case Fmap(f, Mult(q, r)) => List(rewriteLift2(_Lift2(None, ArgsToTuple(f), q, r), lhs)) // f <$> (q <~> r)
              case Ap(_Pure(_, f), Mult(q, r)) => List(rewriteLift2(_Lift2(None, ArgsToTuple(f), q, r), lhs))
              case Ap(Pa(q, _Pure(_, f)), r) => List(rewriteLift2(_Lift2(None, Uncurry(f), q, r), lhs)) // q <**> pure f <*> r
              case Pa(q, Fmap(f, r)) => List(rewriteLift2(_Lift2(None, Flip(Uncurry(f)), q, r), lhs)) // q <**> (f <$> r)
              case Pa(q, Pamf(r, f)) => List(rewriteLift2(_Lift2(None, Flip(Uncurry(f)), q, r), lhs)) // q <**> (f <&> r)
              case Pa(q, Ap(_Pure(_, f), r)) => List(rewriteLift2(_Lift2(None, Flip(Uncurry(f)), q, r), lhs)) // q <**> (pure f <*> r)
              case ApPaOps(q, ops, r) =>
                ops.map {
                  case (op, f) => rewriteLift2(_Lift2(None, Uncurry(f), q, ApR(op, r)), lhs)
                }
                // TODO: maybe generalise with reductions?
              case Fmap(f, DiscardLSeq(`lhs` :: p :: ps)) if p != lhs => List(Right(ConstFmapL(f, (p :: ps).reduce(ApL))))
              case Pamf(DiscardLSeq(`lhs` :: p :: ps), f) if p != lhs => List(Right(ConstFmapL(f, (p :: ps).reduce(ApL))))
              case Pa(`lhs`, ConstFmapL(f, q)) => List(Right(ConstFmapL(f, q)))
              case Pa(`lhs`, ConstFmapR(q, f)) => List(Right(ConstFmapL(f, q)))
              case unknown => List(Left(unknown))
            }
          val (failed, rewritten) = recs.partitionMap(identity)

          val result = (failed, rewritten) match {
            case (Nil, r) => Postfix(None, List(atom, wrapAndReduce(r))) // ideal case; everything is rewritten
            case (f, Nil) => // worst case, nothing is rewritten
              state.addMessage(f"LeftRecursionAnalysis: Rewrites failed for all alternatives of '${printIR(lhs)}':")
              f.foreach(addStateMessage)

              rhs
            case (f, r) =>
              state.addMessage(f"LeftRecursionAnalysis: Rewrites failed for some alternatives of '${printIR(lhs)}':")
              f.foreach(addStateMessage)

              Choice(Postfix(None, List(atom, wrapAndReduce(r))), wrapAndReduce(f))
          }

          // global check
          val globalLeftRecursiveRewrite = rewritten.filter { globalLeftRecursive(lhs, _) }
          if (globalLeftRecursiveRewrite.nonEmpty) {
            state.addMessage(f"LeftRecursionAnalysis: Some rewritten cases for '${printIR(lhs)}' may have non-local left-recursion:")
            globalLeftRecursiveRewrite.foreach(addStateMessage)
          }

          result
      }

      LazyVariableStatement(pLhs, annotation, processE(newRhs))
    case _ => super.processStat(s)
  }

  private object ApPaOps {
    def unapply(ir: IR): Option[(IR, List[(IR, IR)], IR)] = ir match {
      case Ap(Pa(q, ops), r) =>
        val options = c2l(ops).map {
          case ConstFmapR(op, f) => Some((op, f))
          case ConstFmapL(f, op) => Some((op, f))
          case _ => None
        }

        if (options.forall(_.isDefined)) {
          Some((q, options.map(_.get), r))
        } else {
          None
        }
      case _ => None
    }
  }

  private def addStateMessage(e: Expression)(implicit state: IRState): Unit = state.addMessage(f"  - ${printIR(e)}")

  // only applies for lazy?
  private def simpleLeftRecursive(lhs: BindingPattern, rhs: Expression)(implicit state: IRState): Boolean = (lhs, rhs) match {
    case (lhsIR: IR, rhsIR: IR) => localFirst(rhsIR).contains(lhsIR)
    case _ => false
  }

  private def globalLeftRecursive(lhs: BindingPattern, rhs: Expression)(implicit state: IRState): Boolean = (lhs, rhs) match {
    case (lhsIR: IR, rhsIR: IR) =>
      globalElseLocalFirst(rhsIR).contains(lhsIR)
    case _ => false
  }

  private def rewriteLift2(l: Lift2, lhs: BindingPattern)(implicit state: IRState): RewriteResult = {

    /*
    * Note that in lift2, the first parser (p) that runs is the second argument. The end goal for this function is to
    * have p = lhs; termination is ensured as (p) is 'monotonically simplified' with unapply.
    */
    def reduceLift2(l: Lift2): IR = {
      def constructAndReduce(f: IR, p: IR, q: IR): IR = reduceLift2(Lift2(l.typeArgs, List(f, p, q)))

      // if a rewrite changes the first set, it's unsafe? -- probably not true, just an idea?
      l match {
        // case _ :: `lhs` :: _ :: Nil => Right(l)
        case _Lift2(_, f, p, q) => p match {
          case `lhs` => Pamf(q, Flip(Curry(f)))
          case _ if !simpleLeftRecursive(lhs, p) => l // complete
          case ApL(r, s) => constructAndReduce(f, r, ApR(s, q))
          case MultL(r, s) => constructAndReduce(f, r, MultR(s, q))
          case Fmap(g, r) => constructAndReduce(MapFirstArg(g, f), r, q)
          case Pamf(r, g) => constructAndReduce(MapFirstArg(g, f), r, q)
          case ConstFmapL(c, r) => constructAndReduce(MapFirstArg(Const(c), f), r, q)
          case ConstFmapR(r, c) => constructAndReduce(MapFirstArg(Const(c), f), r, q)
          case Mult(r, s) => constructAndReduce(UnpairFirstArg(f), r, Mult(s, q))
          case Label(r, _) => constructAndReduce(f, r, q) // Just drop the label?
          case Pa(`lhs`, s) => constructAndReduce(PaTransform(f), s, q)
          case Pa(r, s) => constructAndReduce(f, Pamf(Mult(r, s), PaFromMult), q)
          case Ap(r, s) => constructAndReduce(f, Pamf(Mult(r, s), ApFromMult), q)
            /*
            * let G be PaFromMult / ApFromMult
            * The above two are equivalent to
            *  => lift2(MapFirstArg(f, G), Mult(r, s), q)
            *  => lift2(UnpairFirstArg(MapFirstArg(f, G)), r, Mult(s, q))
            * As such, the argument holds that each step 'simplifies' the pattern for p
            * */
          case LiftCons(r, s) => constructAndReduce(f, Ap(Fmap(Cons, r), s), q)
          case _: Choice =>
            state.addMessage(f"LeftRecursionAnalysis: Choice combinator not supported for '${printIR(lhs)}'")
            l
          case DiscardRSeq(`lhs` :: `lhs` :: _) => l // Not possible to capture this case?
          case DiscardRSeq(`lhs` :: rest) => Pamf(constructAndReduce(f, rest.reduce(ApR), q), ConstFunction)
          case _: Func =>
            state.addMessage(f"LeftRecursionAnalysis: Function inspection not supported for '${printIR(lhs)}'")
            l
          case _ =>
            state.addMessage(f"LeftRecursionAnalysis: Unhandled case in '${printIR(lhs)}' ($l)")
            l
        }
        case _ => l // incorrect number of args
      }
    }

    // if any rewrite / reduction fails; rollback to original
    val reduced = reduceLift2(l)
    if (simpleLeftRecursive(lhs, reduced)) {
      Left(l)
    } else {
      Right(reduced)
    }
  }

  private def c2l(e: Expression): List[IR] = e match {
    case Choice(l, r) => c2l(l) ++ c2l(r)
    case i: IR => List(i)
    case _ => ???
  }
}
