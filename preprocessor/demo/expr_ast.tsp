type Const = {
  t: 0;
  v: number;
};

type UnOp = {
  t: 1;
  op: string;
  x: Expr;
};

type BinOp = {
    t: 2;
    op: string;
    x: Expr;
    y: Expr;
};

function evaluate(e: Expr): number {
  switch (e.t) {
    case 0: return (<Const>e).v;
    case 1: {
      switch ((<UnOp>e).op) {
        case '+': return evaluate((<UnOp>e).x);
        case '-': return -1 * evaluate((<UnOp>e).x);
      }
    }
    case 2: {
      let ex = evaluate((<BinOp>e).x);
      let ey = evaluate((<BinOp>e).y);
      switch ((<BinOp>e).op) {
        case '+': return ex + ey;
        case '-': return ex - ey;
        case '*': return ex * ey;
        case '/': return ex / ey;
      }
    }
    default: return NaN;
  }
};

type Expr = Const | UnOp | BinOp;
type R = [string, Expr, R] | null;

let digit = satisfy((c: string) => c >= '0' && c <= '9');
let nat = token(many1(digit) <&> (ds: string[]) => parseInt(ds.join(""))) <?> "number";

lazy term: Parser<Expr> =
  nat <&> (x => <Expr>{t: 0, v: x}) <|>
  (cT('+') <|> cT('-')) <~> term <&> (([op, e]) => <Expr>{t: 1, op: op, x: e}) <|>
  cT('(') *> expr <* cT(')');

lazy factor: Parser<Expr> =
  lift2((x, y) => <Expr>{t: 2, op: '*', x: x, y: y}, factor <* cT('*'), term) <|>
  ((x) => (y) => <Expr>{t: 2, op: '/', x: x, y: y}) <$> factor <*> (cT("/") *> term) <|>
  term;

lazy expr: Parser<Expr> =
  (expr <**> (
    (cT("+") $> (x) => (y) => <Expr>{t: 2, op: '+', x: x, y: y}) <|>
    (cT("-") $> (x) => (y) => <Expr>{t: 2, op: '-', x: x, y: y})
  ) <*> factor) <|>
  factor;

let calculator: Parser<number> = many(cT(' ')) *> expr <&> evaluate <* eof;