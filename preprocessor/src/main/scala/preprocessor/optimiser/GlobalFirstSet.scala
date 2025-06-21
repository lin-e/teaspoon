package preprocessor.optimiser

import preprocessor.optimiser.IRNodes._

import scala.collection.mutable
import Utils._

import scala.concurrent.Promise
import scala.util.Success

class GlobalFirstSet(val state: IRState) {
  private val productions = mutable.Map.empty[Id, IR]
  private val firstSets = mutable.Map.empty[Id, mutable.Set[IR]]
  private val globalFirstSet = Promise[Map[Id, Set[IR]]]()

  def addProduction(id: Id, ir: IR): Unit = productions.put(id, ir)

  def compute()(implicit state: IRState): Map[Id, Set[IR]] = {
    if (!globalFirstSet.isCompleted) {
      computeLocal()
      computeGlobal()

      val fs = firstSets
        .map { case k -> v => k -> v.to(Set) }
        .to(Map)

      globalFirstSet.success(fs)
    }

    globalFirstSet.future.value match {
      case Some(Success(fs)) => fs
      case _ => ???
    }
  }

  private def computeLocal()(implicit state: IRState): Unit = {
    productions.foreach { case id -> ir => firstSets.put(id, localFirst(ir).to(mutable.Set)) }
  }

  private def computeGlobal(): Unit = {
    val missing = firstSets.flatMap { case i -> _ =>
      def traverse(ir: IR, seen: Set[Id], acc: Set[IR], missing: Set[Id], addSelf: Boolean = true): (Set[Id], Set[IR], Set[Id]) = {
        val newAcc = acc ++ (if (addSelf) Set(ir) else Set())

        ir match {
          case j: Id if !seen.contains(j) =>
            val updated = seen ++ Set(j)

            firstSets.get(j) match {
              case Some(irs) =>
                irs.foldLeft((updated, newAcc, missing)) { case ((_seen, _acc, _missing), _ir) =>
                  traverse(_ir, _seen, _acc, _missing)
                }
              case None => (updated, newAcc, missing ++ Set(j))
            }
          case _ => (seen, newAcc, missing)
        }
      }

      val (_, traversed, missing) = traverse(i, Set(), Set(), Set(), false)

      firstSets(i).addAll(traversed)

      missing
    }.toSet

    if (missing.nonEmpty) {
      val seq = missing.map { case Id(name) => f"'$name'" }.mkString(", ")
      val pl = missing.size != 1 // plural
      state.addMessage(f"GlobalFirstSet: The identifier${if (pl) "s" else ""} [$seq] ${if (pl) "were" else "was"} used but no declaration was found")
    }
  }
}
