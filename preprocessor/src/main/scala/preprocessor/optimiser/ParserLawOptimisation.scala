package preprocessor.optimiser

import IRNodes._

import scala.annotation.tailrec

object ParserLawOptimisation extends PipelineStage {

  @tailrec
  override def processIR(ir: IR)(implicit state: IRState): IR = super.processIR(ir) match {
    case Choice(p: Pure, _) => p
    case Choice(_: Empty, r) => r
    case Choice(l, _: Empty) => l
    case LiftCons(e: Empty, _) => e
    case Ap(e: Empty, _) => e
    case Pa(e: Empty, _) => e
    case Mult(e: Empty, _) => e
    case ApL(e: Empty, _) => e
    case ApR(e: Empty, _) => e
    case MultL(e: Empty, _) => e
    case MultR(e: Empty, _) => e
    case ApL(l, _: Pure) => l
    case ApR(_: Pure, r) => r
    case MultL(l, _: Pure) => l
    case MultR(_: Pure, r) => r
    case Ap(Pure(_, f :: Nil), p) => processIR(Fmap(f, p))
    case ApL(p, Fmap(_, q)) => processIR(ApL(p, q))
    case ApL(p, ConstFmapL(_, q)) => processIR(ApL(p, q))
    case ApL(p, ConstFmapR(q, _)) => processIR(ApL(p, q))
    case ApL(p, Pamf(q, _)) => processIR(ApL(p, q))
    case MultL(p, Fmap(_, q)) => processIR(MultL(p, q))
    case MultL(p, ConstFmapL(_, q)) => processIR(MultL(p, q))
    case MultL(p, ConstFmapR(q, _)) => processIR(MultL(p, q))
    case MultL(p, Pamf(q, _)) => processIR(MultL(p, q))
    case ApR(Fmap(_, p), q) => processIR(ApR(p, q))
    case ApR(ConstFmapL(_, p), q) => processIR(ApR(p, q))
    case ApR(ConstFmapR(p, _), q) => processIR(ApR(p, q))
    case ApR(Pamf(p, _), q) => processIR(ApR(p, q))
    case MultR(Fmap(_, p), q) => processIR(MultR(p, q))
    case MultR(ConstFmapL(_, p), q) => processIR(MultR(p, q))
    case MultR(ConstFmapR(p, _), q) => processIR(MultR(p, q))
    case MultR(Pamf(p, _), q) => processIR(MultR(p, q))
    case noMatch => noMatch
  }
}
