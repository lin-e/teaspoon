import {
  postfix,
  lazy,
  re,
  apL,
  pamf,
  flip,
  choice,
  Parser,
  apR,
  fmap,
  chr,
  parse,
} from "./combinator";
const nat: Parser<number> = fmap(parseInt, re(/[0-9]+/));
const expr: Parser<number> = lazy(() =>
  postfix(
    term,
    choice(
      pamf(
        apR(chr("+"), term),
        flip((x) => (y) => x + y)
      ),
      pamf(
        apR(chr("-"), term),
        flip((x) => (y) => x - y)
      )
    )
  )
);
const term: Parser<number> = lazy(() =>
  postfix(
    fact,
    choice(
      pamf(
        apR(chr("/"), fact),
        flip((x) => (y) => x / y)
      ),
      pamf(
        apR(chr("*"), fact),
        flip((x) => (y) => x * y)
      )
    )
  )
);
const fact: Parser<number> = lazy(() =>
  choice(nat, apL(apR(chr("("), expr), chr(")")))
);
console.log(parse(expr, "1-2-(1+2)"));
