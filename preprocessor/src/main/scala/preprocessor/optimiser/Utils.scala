package preprocessor.optimiser

import IRNodes._
import preprocessor.parser.ASTNodes._

import scala.annotation.tailrec

object Utils {
  def c2l(i: IR): List[IR] = i match {
    case Choice(l, r) => c2l(l) ++ c2l(r)
    case _ => List(i)
  }

  def globalElseLocalFirst(ir: IR)(implicit state: IRState): Set[IR] = ir match {
    case i: Id => state.computedGFS().getOrElse(i, Set(i))
    case _ =>
      localFirst(ir).flatMap {
        case i: Id => state.computedGFS().getOrElse(i, Set(i))
        case lf => Set(lf)
      }
  }

  @tailrec
  def stripAttempt(ir: IR): IR = ir match {
    case Attempt(_, p :: Nil) => stripAttempt(p)
    case _ => ir
  }

  def localFirst(ir: IR)(implicit state: IRState): Set[IR] = state.getLocalFirst(ir) match {
    case Some(value) => value
    case None =>
      val result: Set[IR] = ir match {
        case irl: IRLiteral =>
          val v = irl.l.value
          val head: Set[IR] = if (stringLiteral(v)) {
            irl.l.value.toList.drop(1).dropRight(1) match {
              case EscapeSequence(e, _) => Set(LeadingChar(e))
              case c :: _ => Set(LeadingChar(c.toString))
              case _ => Set()
            }
          } else {
            Set()
          }
          head | Set(irl)
        case a: Atom => Set(a)
        case i: Id => Set(i)
        case combinator: Combinator => combinator match {
          case Choice(l, r) => localFirst(l) | localFirst(r)
          case _: Fmap | _: ConstFmapL => localFirst(combinator.r)
          case _ => combinator.l match {
            case _: Pure => localFirst(combinator.r)
            case _ =>
              val lf = localFirst(combinator.l)

              // handles the case where the LHS is a combinator consisting of all pures
              if (lf.isEmpty) {
                localFirst(combinator.r)
              } else {
                lf
              }
          }
        }
        case func: Func => func match {
          case _: Lift2 | _: Lift3 | _: Lift4 | _: Lift5 | _: Lift6 | _: Lift7 | _: Lift8 | _: Lift9 => func.args match {
            case _ :: arg :: _ => localFirst(arg)
            case _ => Set(func)
          }
          case _: Pure | _: Empty => Set()
          case _: Satisfy | _: Re => Set(func)
          case _: Chr | _: Str | _: CT => func.args match {
            case (irl: IRLiteral) :: Nil => localFirst(irl) | Set(func)
            case _ => Set(func)
          }
          case _ => func.args match {
            case arg :: _ => localFirst(arg)
            case _ => Set(func)
          }
        }
        case g: Generated => Set(g) // Shouldn't ever really get to this case
      }
      state.setLocalFirst(ir, result)
      result
  }

  def printIR(ir: AST)(implicit state: IRState): String = {
    state.lock()
    val s = ir match {
      case bp: BindingPattern => FromIR.processBPatt(bp).print()
      case e: Expression => FromIR.processE(e).print()
      case _ => ???
    }
    state.unlock()

    s
  }

  def charLiteral(v: String): Boolean = {
    stringLiteral(v) && (v.drop(1).dropRight(1).toList match {
      case EscapeSequence(_, Nil) => true
      case c => c.length == 1
    })
  }
  def stringLiteral(v: String): Boolean = (v.startsWith("\"") && v.endsWith("\"")) || (v.startsWith("'") && v.endsWith("'")) && v.length > 1
  def regexLiteral(v: String): Boolean = v.startsWith("/") // only possible for a Literal(_) to begin with '/' when it is a regex literal

  object EscapeSequence {
    def unapply(cs: List[Char]): Option[(String, List[Char])] = cs match {
      case '\\' :: r => r match {
        case 'x' :: h1 :: h2 :: rest if hex(h1, h2) => Some((f"\\x$h1$h2", rest))
        case 'u' :: h1 :: h2 :: h3 :: h4 :: rest if hex(h1, h2, h3, h4) => Some((f"\\u$h1$h2$h3$h4", rest))
        case 'u' :: '{' :: rr =>
          val (hexChars, afterHex) = rr.span(hex)
          afterHex match {
            case '}' :: rest => Some((f"\\u{${hexChars.mkString}}", rest))
            case _ => None
          }
        case '0' :: rest => Some("\\0", rest)
        case c :: rest if sec(c) || nec(c) => Some((f"\\$c", rest))
        case _ => None // ???
      }
      case _ => None
    }

    // Copied from parser.TypeScript

    // single escape character
    def sec(c: Char): Boolean = Set('\'', '"', '\\', 'b', 'f', 'n', 'r', 't', 'v').contains(c)

    // non escape character
    def nec(c: Char): Boolean = c match {
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => false
      case '\'' | '"' | '\\' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' => false
      case 'x' | 'u' => false
      case _ => true
    }

    def hex(cs: Char*): Boolean = cs.forall(hex)
    def hex(c: Char): Boolean = c match {
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => true
      case 'a' | 'b' | 'c' | 'd' | 'e' | 'f' => true
      case 'A' | 'B' | 'C' | 'D' | 'E' | 'F' => true
      case _ => false
    }
  }

  // Patterns that are used often.

  object _Lift2 {
    def unapply(ir: IR): Option[(Option[TypeArguments], IR, IR, IR)] = ir match {
      case Lift2(t, f :: p :: q :: Nil) => Some((t, f, p, q))
      case _ => None
    }

    def apply(t: Option[TypeArguments], f: IR, p: IR, q: IR): Lift2 = Lift2(t, List(f, p, q))
  }

  object _Pure {
    def unapply(ir: IR): Option[(Option[TypeArguments], IR)] = ir match {
      case Pure(t, x :: Nil) => Some((t, x))
      case _ => None
    }

    def apply(t: Option[TypeArguments], x: IR): Pure = Pure(t, List(x))
  }

  object DiscardRSeq {
    def unapply(ir: IR): Option[List[IR]] = discardRChain(ir) match {
      case Nil | _ :: Nil => None
      case chain => Some(chain)
    }

    def discardRChain(ir: IR): List[IR] = ir match {
      case ApR(l, r) => discardRChain(l) ++ discardRChain(r)
      case MultR(l, r) => discardRChain(l) ++ discardRChain(r)
      case _ => List(ir)
    }
  }

  object DiscardLSeq {
    def unapply(ir: IR): Option[List[IR]] = discardLChain(ir) match {
      case Nil | _ :: Nil => None
      case chain => Some(chain)
    }

    def discardLChain(ir: IR): List[IR] = ir match {
      case ApL(l, r) => discardLChain(l) ++ discardLChain(r)
      case MultL(l, r) => discardLChain(l) ++ discardLChain(r)
      case _ => List(ir)
    }
  }
}
