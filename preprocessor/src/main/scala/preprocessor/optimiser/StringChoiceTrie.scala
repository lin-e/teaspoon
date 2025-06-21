package preprocessor.optimiser

import IRNodes._
import preprocessor.parser.ASTNodes._
import Utils._

import scala.annotation.tailrec

object StringChoiceTrie extends PipelineStage {
  private val MinimumChoiceLength = 2

  override def processIR(ir: IR)(implicit state: IRState): IR = ir match {
    case StringChoiceSequence(ss) =>
      val ws = ss.map(_.toList)
      val t = createTrie(ws).compress()
      val ct = t.createCombinator()
      ct match {
        case LiftStringConcat(_, rem) => rem
        case _ => ??? // Always drop root
      }
    case _ => super.processIR(ir)
  }

  def main(args: Array[String]): Unit = {
    val ws = List("and", "anthem", "anti", "an").map(_.toList)
    val t = createTrie(ws)
    for (w <- ws) {
      println(f"search($w) => ${t.search(w)}")
    }
  }

  case class CTrie(value: String, children: Map[String, CTrie] = Map.empty, word: Boolean = false, root: Boolean = false) {
    def insert(cs: List[Char]): CTrie = cs match {
      case EscapeSequence(s, rest) => updateChild(s, rest)
      case c :: rest => updateChild(c.toString, rest)
      case Nil => this.copy(word = true)
    }

    private def updateChild(s: String, rest: List[Char]): CTrie = {
      val updated = s -> children.getOrElse(s, CTrie(s)).insert(rest)
      this.copy(children = children + updated)
    }

    @tailrec
    final def search(cs: List[Char]): Boolean = cs match {
      case c :: rest => children.get(c.toString) match {
        case Some(value) => value.search(rest)
        case None => false
      }
      case Nil => word
    }

    def compress(): CTrie = children.toList match {
      case (_ -> t) :: Nil if !word && !root =>
        val cc = t.compress() // compressed child
        cc.copy(value = value + cc.value)
      case _ => this.copy(children = children.map { case s -> t => s -> t.compress() } )
    }

    def createCombinator(): IR = {
      val leaf = (if (value.length == 1) Chr else Str)(None, List(Explicit(Literal(f"\"$value\""))))
      if (children.isEmpty) {
        leaf
      } else {
        val cs = children.map { case _ -> t => t.createCombinator() } ++ (if (word) List(StringEpsilon) else Nil)
        LiftStringConcat(leaf, cs.reduce(Choice))
      }
    }
  }

  private object StringChoiceSequence {
    def unapply(i: IR)(implicit state: IRState): Option[List[List[Char]]] = {
      def stringFromAtom(a: Atom): Option[List[Char]] = state.getExpr(a) match {
        case Literal(v) if stringLiteral(v) => Some(v.drop(1).dropRight(1).toList)
        case _ => None
      }
      def stringFromLiteral(irl: IRLiteral): Option[List[Char]] = {
        val v = irl.l.value
        if (stringLiteral(v)) {
          Some(v.drop(1).dropRight(1).toList)
        } else {
          None
        }
      }

      val choices = c2l(i)
      val strs = choices
        .map(stripAttempt)
        .flatMap {
          case Str(None, (a: Atom) :: Nil) => stringFromAtom(a)
          case Chr(None, (a: Atom) :: Nil) => stringFromAtom(a)
          case Str(None, (irl: IRLiteral) :: Nil) => stringFromLiteral(irl)
          case Chr(None, (irl: IRLiteral) :: Nil) => stringFromLiteral(irl)
          case _ => None
        }

      if ((strs.length == choices.length) && (strs.length >= MinimumChoiceLength)) {
        Some(strs)
      } else {
        None
      }
    }
  }

  private def createTrie(words: List[List[Char]]): CTrie = words.foldLeft(CTrie("", root = true))((t, cs) => t.insert(cs))
}
