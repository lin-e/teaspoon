package preprocessor.optimiser

import preprocessor.optimiser.IRNodes._
import preprocessor.Common
import Utils._

import scala.annotation.tailrec
import scala.collection.mutable

object BacktrackingReduction extends PipelineStage {
  private val MinimumChoiceLength = 2

  sealed trait FlatIR

  case class P(p: IR) extends FlatIR

  case class C(c: String) extends FlatIR

  case object X extends FlatIR // placeholder

  type FlatParser = List[FlatIR]

  sealed trait Group

  case class Split(prefix: IR, op: (IR, IR) => IR, suffixes: List[IR]) extends Group

  case class Atomic(ir: IR) extends Group

  override def processIR(ir: IR)(implicit state: IRState): IR = ir match {
    case AttemptChoiceSequence(as) => optimise(as)
    case _ => super.processIR(ir)
  }

  // Creates a normal form to maximise ability to factor
  private object LeftAssociate extends PipelineStage {
    override def processIR(ir: IR)(implicit state: IRState): IR = ir match {
      case DiscardL(l, DiscardL(rl, rr)) => processIR(ApL(processIR(ApL(l, rl)), rr))
      case DiscardR(l, DiscardR(rl, rr)) => processIR(ApR(processIR(ApR(l, rl)), rr))
      case DiscardR(p, Ap(q, r)) => processIR(Ap(processIR(ApR(p, q)), r))
      case Ap(p, DiscardL(q, r)) => processIR(ApL(processIR(Ap(p, q)), r))
      case MultL(p, q) => processIR(ApL(p, q))
      case MultR(p, q) => processIR(ApR(p, q))
      case _ => super.processIR(ir)
    }

    object DiscardL {
      def unapply(ir: IR): Option[(IR, IR)] = ir match {
        case ApL(l, r) => Some((l, r))
        case MultL(l, r) => Some((l, r))
        case _ => None
      }
    }

    object DiscardR {
      def unapply(ir: IR): Option[(IR, IR)] = ir match {
        case ApR(l, r) => Some((l, r))
        case MultR(l, r) => Some((l, r))
        case _ => None
      }
    }
  }

  private def optimise(irs: List[IR])(implicit state: IRState): IR = {
    val reassoc = irs.map(LeftAssociate.processIR(_))
    val fusedIRs = fuseMerkle(reassoc)
    orderIR(fusedIRs)
  }

  private case class AwaitingFusion(irs: List[IR]) extends IR

  private def fuseMerkle(irs: List[IR])(implicit state: IRState): List[IR] = {
    val fuseCandidates = mutable.Set.empty[IR]

    // a in fuseCandidates, b in irs
    def merkleMatch(a: IR, b: IR, first: Boolean = false): Option[IR] = {
      (a, b) match {
        case _ if a == b => Some(a)
        case (ac: Combinator, bc: Combinator) if cToOp(ac) == cToOp(bc) => // same combinator
          val (fuseL, fuseR) = ac match {
            case _: Choice => (false, false)
            case _: Fmap | _: ConstFmapL => (false, true)
            case _: Pamf | _: Label | _: ConstFmapR => (true, false)
            case _ => (true, true)
          }
          val CombinatorIR(op) = cToOp(ac)

          // cannot both be true, otherwise the combinators would be equal
          val (matchL, matchR) = (ac.l == bc.l, ac.r == bc.r)

          (fuseL, fuseR) match {
            case (true, true) if matchL => merkleMatch(ac.r, bc.r).map(op(ac.l, _))
//            case (true, true) if matchR => merkleMatch(ac.l, bc.l).map(op(_, ac.r))
//            case (true, false) if matchR => merkleMatch(ac.l, bc.l).map(op(_, ac.r))
            case (false, true) if matchL => merkleMatch(ac.r, bc.r).map(op(ac.l, _))
            case _ if first => None
            case _ => Some(AwaitingFusion(List(a, b)))
          }
        case (AwaitingFusion(irs), b) => Some(AwaitingFusion(b :: irs))
        case _ if first => None
        case _ => Some(AwaitingFusion(List(a, b)))
      }
    }

    irs.foreach { ir =>
      val firstFuse = fuseCandidates
        .map { a => (a, merkleMatch(a, ir, true)) }
        .collectFirst { case (a, Some(fused)) => (a, fused) }

      firstFuse match {
        case Some((a, fused)) =>
          fuseCandidates.remove(a)
          fuseCandidates.addOne(fused)
        case None => fuseCandidates.addOne(ir)
      }
    }

    object ReduceFusion extends PipelineStage {
      override def processIR(ir: IR)(implicit state: IRState): IR = ir match {
        case AwaitingFusion(irs) => optimise(irs.map(processIR))
        case _ => super.processIR(ir)
      }
    }

    fuseCandidates
      .map(ReduceFusion.processIR)
      .toList
  }

  // Converts groups to an IR, also handles removal of redundant attempts
  private def orderIR(irs: List[IR])(implicit state: IRState): IR = {
    def parserSeq(ir: IR): List[IR] = {
      // Similar to peephole optimisation?
      def peepSeq(fp: FlatParser): List[IR] = fp match {
        case Nil => Nil
        case _ :: C("<$>") :: rem => peepSeq(rem)
        case _ :: C("<$") :: rem => peepSeq(rem)
        case Consumes(p) :: C("<&>") :: _ :: rem => p :: peepSeq(rem)
        case Consumes(p) :: C("$>") :: _ :: rem => p :: peepSeq(rem)
        case Consumes(p) :: rem => p :: peepSeq(rem)
        case _ :: rem => peepSeq(rem)
      }

      object Consumes {
        def unapply(p: P): Option[IR] = p match {
          case P(q) if localFirst(q).nonEmpty => Some(q)
          case _ => None
        }
      }

      peepSeq(irToFIR(ir))
    }

    def collides(as: Set[IR], bs: Set[IR]): Boolean = {
      def matches(a: IR, b: IR): Boolean = {
        if (a == b) {
          true
        } else {
          def isLiteral(i: IR) = i match {
            case IRLiteral(l) => stringLiteral(l.value) && l.value.length > 2
            case _ => false
          }
          def hardToCheck(i: IR): Boolean = i match {
            case _: Satisfy | _: Re => true
            case _: Atom | _: Id => true
            case Str(_, args) => !args.forall(isLiteral)
            case Chr(_, args) => !args.forall(isLiteral)
            case _ => false
          }

          hardToCheck(a) || hardToCheck(b)
        }
      }

      as.exists { a => bs.exists { b => matches(a, b) } }
    }

    val firstSets = irs
      .map { i => i -> globalElseLocalFirst(i) }
      .toMap

    val t = ITrie()

    irs.foreach { i =>
      val fs = firstSets(i)
      val firstSetCollision = firstSets
        .filterNot { case ir -> _ => ir == i }
        .exists { case _ -> o => collides(fs, o) }

      if (firstSetCollision) {
        t.insert(parserSeq(i), Attempt(None, List(i)))
      } else {
        t.insert(parserSeq(i), i)
      }
    }

    t.ordered().reduce(Choice)
  }

  // needs to be mutable to preserve insertion order
  case class ITrie(children: mutable.Map[IR, ITrie] = mutable.LinkedHashMap.empty, complete: mutable.Buffer[IR] = mutable.ListBuffer.empty) {
    @tailrec
    final def insert(ps: Seq[IR], prod: IR): Unit = ps match {
      case p :: rest =>
        if (!children.contains(p)) {
          children.addOne(p, ITrie())
        }
        children(p).insert(rest, prod)
      case Nil => complete.append(prod)
    }

    // perform a post-order traversal
    def ordered(): List[IR] = children.flatMap { case _ -> t => t.ordered() }.toList ++ complete
  }


  private object CombinatorIR {
    def unapply(c: String): Option[(IR, IR) => IR] = Common.customOperators.get(c).map(_.ir)
  }

  private object AttemptChoiceSequence {
    def unapply(i: IR)(implicit state: IRState): Option[List[IR]] = {
      val choices = c2l(i)
      val attempts = choices.flatMap {
        case Attempt(None, p :: Nil) => Some(stripAttempt(p))
        case _ => None
      }

      if ((attempts.length == choices.length) && (attempts.length >= MinimumChoiceLength)) {
        Some(attempts)
      } else {
        None
      }
    }
  }

  private def cToOp(c: Combinator): String = c match {
    case _: Ap => "<*>"
    case _: Pa => "<**>"
    case _: ApL => "<*"
    case _: ApR => "*>"
    case _: Choice => "<|>"
    case _: Mult => "<~>"
    case _: MultL => "<~"
    case _: MultR => "~>"
    case _: Fmap => "<$>"
    case _: Pamf => "<&>"
    case _: ConstFmapL => "<$"
    case _: ConstFmapR => "$>"
    case _: LiftCons => "<:>"
    case _: Label => "<?>"
  }

  private def irToFIR(i: IR): FlatParser = i match {
    case c: Combinator => c match {
      case _: Choice => List(P(c))
      case _: Fmap | _: ConstFmapL => P(c.l) :: C(cToOp(c)) :: irToFIR(c.r)
      case _: Pamf | _: ConstFmapR => irToFIR(c.l) ++ (C(cToOp(c)) :: List(P(c.r)))
      case _ => irToFIR(c.l) ++ (C(cToOp(c)) :: irToFIR(c.r))
    }
    case _ => List(P(i))
  }
}
