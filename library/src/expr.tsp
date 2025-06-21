inline add = x => y => x + y;
inline sub = x => y => x - y;
inline mul = x => y => x * y;
inline div = x => y => x / y;

val  nat: Parser<number>  = parseInt <$> /[0-9]+/;
lazy expr: Parser<number> = expr <**> ('+' $> add) <*> term <|>
            expr <**> ('-' $> sub) <*> term <|>
            term;
lazy term: Parser<number> = term <**> ('*' $> mul) <*> fact <|>
            term <**> ('/' $> div) <*> fact <|>
            fact;
lazy fact: Parser<number> = nat <|>
            '(' *> expr <* ')';


console.log(parse(expr, "1+2"));