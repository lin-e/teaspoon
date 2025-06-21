package preprocessor.optimiser

import preprocessor.optimiser.IRNodes._
import preprocessor.parser.ASTNodes.Expression

import scala.collection.mutable

class IRState {
  type Message = String

  private val atoms = mutable.Map.empty[Expression, Int]
  private val swapped = mutable.Map.empty[Int, Expression]
  private val messages = mutable.ListBuffer.empty[Message]
  private val imports = mutable.Set("Parser", "parse")
  private val gfs = new GlobalFirstSet(this)
  private val localFirstMemo = mutable.Map.empty[IR, Set[IR]]

  private var next = 0
  private var locked = false

  def getAtom(e: Expression): Atom = {
    if (locked) {
      Atom(-1)
    } else {
      atoms.get(e) match {
        case Some(value) => Atom(value)
        case None =>
          val id = next
          atoms.put(e, id)
          swapped.put(id, e)

          next += 1

          Atom(id)
      }
    }
  }

  def getLocalFirst(ir: IR): Option[Set[IR]] = localFirstMemo.get(ir)

  def setLocalFirst(ir: IR, lf: Set[IR]): Unit = if (!locked) localFirstMemo.addOne(ir, lf)

  def getExpr(a: Atom): Expression = swapped.getOrElse(a.id, ???)

  def addMessage(m: Message): Unit = if (!locked) messages.addOne(m)

  def getMessages: List[Message] = messages.toList

  def addImport(i: String): Unit = if (!locked) imports.add(i)

  def getImports: Set[String] = imports.toSet

  def addProduction(id: Id, ir: IR): Unit = if (!locked) gfs.addProduction(id, ir)

  def computedGFS(): Map[Id, Set[IR]] = if (!locked) gfs.compute()(this) else Map.empty

  // Prevent the state from being modified
  def lock(): Unit = locked = true
  def unlock(): Unit = locked = false
}
