package preprocessor

import preprocessor.optimiser.IRNodes._
import preprocessor.parser.ASTNodes._

import scala.collection.immutable.ListMap

object Common {

  case class CustomOperator(f: String, precedence: Int, ir: (IR, IR) => IR)
  case class ReservedFunction(parserArgs: Set[Int], ir: (Option[TypeArguments], List[IR]) => IR)

  val customOperators: Map[String, CustomOperator] = ListMap(
    "<*>" -> CustomOperator("ap", 4, Ap),
    "<**>" -> CustomOperator("pa", 4, Pa),
    "<*" -> CustomOperator("apL", 4, ApL),
    "*>" -> CustomOperator("apR", 4, ApR),
    "<|>" -> CustomOperator("choice", 3, Choice),
    "<~>" -> CustomOperator("mult", 4, Mult),
    "<~" -> CustomOperator("multL", 4, MultL),
    "~>" -> CustomOperator("multR", 4, MultR),
    "<$>" -> CustomOperator("fmap", 4, Fmap),
    "<&>" -> CustomOperator("pamf", 4, Pamf),
    "<$" -> CustomOperator("constFmapL", 4, ConstFmapL),
    "$>" -> CustomOperator("constFmapR", 4, ConstFmapR),
    "<:>" -> CustomOperator("liftCons", 4, LiftCons),
    "<?>" -> CustomOperator("label", 0, Label),
  )

  val reservedFunctions: Map[String, ReservedFunction] = ListMap(
    "pure" -> ReservedFunction(Set(), Pure),
    "satisfy" -> ReservedFunction(Set(), Satisfy),
    "chr" -> ReservedFunction(Set(), Chr),
    "str" -> ReservedFunction(Set(), Str),
    "many1" -> ReservedFunction(Set(0), Many1),
    "many" -> ReservedFunction(Set(0), Many),
    "attempt" -> ReservedFunction(Set(0), Attempt),
    "token" -> ReservedFunction(Set(0), Token),
    "cT" -> ReservedFunction(Set(), CT),
    "chainl1" -> ReservedFunction(Set(0, 1), ChainL1),
    "chainr1" -> ReservedFunction(Set(0, 1), ChainR1),
    "lift2" -> ReservedFunction(Set(1, 2), Lift2),
    "lift3" -> ReservedFunction(Set(1, 2, 3), Lift3),
    "lift4" -> ReservedFunction((1 to 4).toSet, Lift4),
    "lift5" -> ReservedFunction((1 to 5).toSet, Lift5),
    "lift6" -> ReservedFunction((1 to 6).toSet, Lift6),
    "lift7" -> ReservedFunction((1 to 7).toSet, Lift7),
    "lift8" -> ReservedFunction((1 to 8).toSet, Lift8),
    "lift9" -> ReservedFunction((1 to 9).toSet, Lift9),
    "postfix" -> ReservedFunction(Set(0, 1), Postfix),
    "empty" -> ReservedFunction(Set(), Empty),
    "between" -> ReservedFunction(Set(0, 1, 2), Between),
    "re" -> ReservedFunction(Set(), Re),
  )

  val expressionParser: String =
    """type Const = {
      |  t: 0;
      |  v: number;
      |};
      |
      |type UnOp = {
      |  t: 1;
      |  op: string;
      |  x: Expr;
      |};
      |
      |type BinOp = {
      |    t: 2;
      |    op: string;
      |    x: Expr;
      |    y: Expr;
      |};
      |
      |let abc = token(p <|> p) <|> q <|> r <|> r;
      |
      |function combine(x: Expr, y: R): Expr {
      |  if (y == null) {
      |    return x;
      |  }
      |
      |  let z: BinOp = {
      |    t: 2,
      |    op: y[0],
      |    x: x,
      |    y: y[1]
      |  };
      |  return combine(z, y[2]);
      |}
      |
      |function evaluate(e: Expr): number {
      |  switch (e.t) {
      |    case 0: return (<Const>e).v;
      |    case 1: {
      |      switch ((<UnOp>e).op) {
      |        case '+': return evaluate((<UnOp>e).x);
      |        case '-': return -1 * evaluate((<UnOp>e).x);
      |      }
      |    }
      |    case 2: {
      |      let ex = evaluate((<BinOp>e).x);
      |      let ey = evaluate((<BinOp>e).y);
      |      switch ((<BinOp>e).op) {
      |        case '+': return ex + ey;
      |        case '-': return ex - ey;
      |        case '*': return ex * ey;
      |        case '/': return ex / ey;
      |      }
      |    }
      |    default: return NaN;
      |  }
      |}
      |
      |type Expr = Const | UnOp | BinOp;
      |type R = [string, Expr, R] | null;
      |
      |let digit = satisfy((c: string) => c >= '0' && c <= '9');
      |let nat = token(many1(digit) <&> (ds: string[]) => parseInt(ds.join(""))) <?> "number";
      |
      |lazy term: Parser<Expr> =
      |  nat <&> (x => <Expr>{t: 0, v: x}) <|>
      |  (cT('+') <|> cT('-')) <~> term <&> (([op, e]) => <Expr>{t: 1, op: op, x: e}) <|>
      |  cT('(') *> expr <* cT(')');
      |
      |lazy factorR: Parser<R> =
      |  (cT('*') <|> cT('/')) <~> (term <~> factorR) <&> (([op, [e, r]]) => [op, e, r]) <|>
      |  epsilon;
      |
      |let factor: Parser<Expr> = term <~> factorR <&> ([e, r]) => combine(e, r);
      |
      |lazy exprR: Parser<R> =
      |  (cT('+') <|> cT('-')) <~> (factor <~> exprR) <&> (([op, [e, r]]) => [op, e, r]) <|>
      |  epsilon;
      |
      |let expr: Parser<Expr> = factor <~> exprR <&> ([e, r]) => combine(e, r);
      |
      |let calculator: Parser<number> = many(cT(' ')) *> expr <&> evaluate <* eof;
      |""".stripMargin

  // Contains left recursion!
  val expressionParserLR: String =
    """type Const = {
      |  t: 0;
      |  v: number;
      |};
      |
      |type UnOp = {
      |  t: 1;
      |  op: string;
      |  x: Expr;
      |};
      |
      |type BinOp = {
      |    t: 2;
      |    op: string;
      |    x: Expr;
      |    y: Expr;
      |};
      |
      |function evaluate(e: Expr): number {
      |  switch (e.t) {
      |    case 0: return (<Const>e).v;
      |    case 1: {
      |      switch ((<UnOp>e).op) {
      |        case '+': return evaluate((<UnOp>e).x);
      |        case '-': return -1 * evaluate((<UnOp>e).x);
      |      }
      |    }
      |    case 2: {
      |      let ex = evaluate((<BinOp>e).x);
      |      let ey = evaluate((<BinOp>e).y);
      |      switch ((<BinOp>e).op) {
      |        case '+': return ex + ey;
      |        case '-': return ex - ey;
      |        case '*': return ex * ey;
      |        case '/': return ex / ey;
      |      }
      |    }
      |    default: return NaN;
      |  }
      |};
      |
      |type Expr = Const | UnOp | BinOp;
      |type R = [string, Expr, R] | null;
      |
      |let digit = satisfy((c: string) => c >= '0' && c <= '9');
      |let nat = token(many1(digit) <&> (ds: string[]) => parseInt(ds.join(""))) <?> "number";
      |
      |lazy term: Parser<Expr> =
      |  nat <&> (x => <Expr>{t: 0, v: x}) <|>
      |  (cT('+') <|> cT('-')) <~> term <&> (([op, e]) => <Expr>{t: 1, op: op, x: e}) <|>
      |  cT('(') *> expr <* cT(')');
      |
      |lazy factor: Parser<Expr> =
      |  lift2((x, y) => <Expr>{t: 2, op: '*', x: x, y: y}, factor <* cT('*'), term) <|>
      |  ((x) => (y) => <Expr>{t: 2, op: '/', x: x, y: y}) <$> factor <*> (cT("/") *> term) <|>
      |  term;
      |
      |lazy expr: Parser<Expr> =
      |  (expr <**> (
      |    (cT("+") $> (x) => (y) => <Expr>{t: 2, op: '+', x: x, y: y}) <|>
      |    (cT("-") $> (x) => (y) => <Expr>{t: 2, op: '-', x: x, y: y})
      |  ) <*> factor) <|>
      |  factor;
      |
      |let calculator: Parser<number> = many(cT(' ')) *> expr <&> evaluate <* eof;
      |""".stripMargin
}
