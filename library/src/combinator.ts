export type State = {
    rem: string;
    pos: number;
};

export type Message = {
    pos: number;
    unexpected: string;
    expected: string[];
};

export type Ok<A> = {
    success: true;
    tree: A;
    state: State;
    msg: Message;
};

export type Err = {
    success: false;
    msg: Message;
};

// export type Reply<A> = Ok<A> | Err;

export type Result<A> = {
    consumed: boolean;
    reply: Ok<A> | Err;
};

export type Parser<A> = (s: State) => Result<A>

export type ParseSuccess<A> = {
    success: true;
    consumed: boolean;
    tree: A;
};

export type ParseFail = {
    success: false;
    consumed: boolean;
    pos: number;
    unexpected: string;
    expected: string[];
};

export type ParseResult<A> = ParseSuccess<A> | ParseFail;

export function parse<A>(p: Parser<A>, input: string): ParseResult<A> {
    let pRes = p({rem: input, pos: 0});
    if (pRes.reply.success) {
        return {
            success: true,
            consumed: pRes.consumed,
            tree: pRes.reply.tree
        };
    } else {
        return {
            success: false,
            consumed: pRes.consumed,
            pos: pRes.reply.msg.pos,
            unexpected: pRes.reply.msg.unexpected,
            expected: pRes.reply.msg.expected
        };
    }
}

// utility functions
export function _const<A>(x: A): (_: any) => A {
    return (_: any) => x
}

export function id<A>(x: A): A {
    return x;
}

export function cons<A>(x: A): (xs: A[]) => A[] {
    return (xs: A[]) => [x, ...xs];
}
// export function flip<A, B, C>(f: (x: A, y: B) => C): (y: B, x: A) => C {
//     return (y: B, x: A) => f(x, y)
// }

export function flip<A, B, C>(f: (a: A) => (b: B) => C): (b: B) => (a: A) => C {
    return (b: B) => (a: A) => f(a)(b);
}

// generate simple empty error
function failed<A>(pos: number, unexpected: string, expected: string[] = []): Result<A> {
    return {
        consumed: false,
        reply: {
            success: false,
            msg: {
                pos: pos,
                unexpected: unexpected,
                expected: expected
            }
        }
    }
}

function failedMsg<A>(msg: Message): Result<A> {
    return failed(msg.pos, msg.unexpected, msg.expected);
}

// rudimentary support for laziness
export function lazy_old<A>(p: () => Parser<A>): Parser<A> {
    return (s: State) => p()(s);
}

export function lazy<A>(p: () => Parser<A>): Parser<A> {
    const l = new _Lazy(p);
    return (s: State) => l.p()(s);
}

class _Lazy<T> {
    private _f: () => T;
    private _p: T | undefined;
    constructor (f: () => T) {
        this._f = f;
    }

    public p(): T {
        return (this._p === undefined) ? (this._p = this._f()) : this._p;
    }
}

// return / pure from Monad / Applicative
export function pure<A>(x: A): Parser<A> {
    return (s: State) => ({
        consumed: false,
        reply: {
            success: true,
            tree: x,
            state: s,
            msg: {
                pos: s.pos,
                unexpected: "",
                expected: []
            }
        }
    })
}

// primitive parser, also part of Alternative
export function empty<A>(): Parser<A> {
    return (s: State) => failed(s.pos, "empty");
}

// primitive parser
export let item: Parser<string> = (s: State) => {
    if (!s.rem || s.rem.length == 0) {
        return failed(s.pos, "End of input");
    } else {
        return {
            consumed: true,
            reply: {
                success: true,
                tree: s.rem.slice(0, 1),
                state: {
                    rem: s.rem.slice(1),
                    pos: s.pos + 1
                },
                msg: {
                    pos: s.pos,
                    unexpected: "",
                    expected: []
                }
            }
        }
    }
};

// basic epsilon parser
export let epsilon: Parser<null> = pure(null);

// eof parser
export let eof: Parser<null> = (s: State) => {
    if (!s.rem || s.rem.length == 0) {
        return epsilon(s);
    }

    return {
        consumed: false,
        reply: {
            success: false,
            msg: {
                pos: s.pos,
                unexpected: s.rem.slice(0, 1),
                expected: ["eof"]
            }
        }
    }
}

// 'primitive' parser
// expects the test to require c.length == 1
export function satisfy(test: (c: string) => boolean): Parser<string> {
    return (s: State) => {
        if (!s.rem || s.rem.length == 0) {
            return failed(s.pos, "End of input");
        } else {
            let c = s.rem.slice(0, 1);
            if (test(c)) {
                return {
                    consumed: true,
                    reply: {
                        success: true,
                        tree: c,
                        state: {
                            rem: s.rem.slice(1),
                            pos: s.pos + 1
                        },
                        msg: {
                            pos: s.pos,
                            unexpected: "",
                            expected: []
                        }
                    }
                }
            } else {
                return failed(s.pos, c);
            }
        }
    }
}

export function chr(c: string): Parser<string> {
    return label(satisfy(chr => chr == c), c);
}

export function str(s: string): Parser<string> {
    if (!s || s.length == 0) {
        return pure("");
    }

    return bind(chr(s.slice(0, 1)), c => bind(str(s.slice(1)), cs => pure(c + cs)))
}

export function re(rexp: RegExp): Parser<string> {
    return (s: State) => {
        let substring = s.rem;
        let match = new RegExp("^" + rexp.source, rexp.flags).exec(substring);
        if (match) {
            let r = match[0];
            return {
                consumed: r.length > 0,
                reply: {
                    success: true,
                    tree: r,
                    state: {
                        rem: s.rem.slice(r.length),
                        pos: s.pos + r.length
                    },
                    msg: {
                        pos: s.pos,
                        unexpected: "",
                        expected: []
                    }
                }
            }
        } else {
            return failed(s.pos, substring[0], ["string matching " + rexp.source]);
        }
    }
}

// Parsec's message merging
function merge(m1: Message, m2: Message): Message {
    return {
        pos: m1.pos,
        unexpected: m1.unexpected,
        expected: m1.expected.concat(m2.expected)
    }
}

function mergeErr<A>(m1: Message, m2: Message): Result<A> {
    return {
        consumed: false,
        reply: {
            success: false,
            msg: merge(m1, m2)
        }
    }
}

function mergeOk<A>(tree: A, state: State, m1: Message, m2: Message): Result<A> {
    return {
        consumed: false,
        reply: {
            success: true,
            tree: tree,
            state: state,
            msg: merge(m1, m2)
        }
    }
}

// (>>=) from Monad
export function bind<A, B>(p: Parser<A>, f: (a: A) => Parser<B>): Parser<B> {
    return (s: State) => {
        let pRes = p(s);
        if (pRes.reply.success) {
            let qRes = f(pRes.reply.tree)(pRes.reply.state);
            if (qRes.reply.success) {
                return {
                    consumed: pRes.consumed || qRes.consumed,
                    reply: {
                        success: true,
                        tree: qRes.reply.tree,
                        state: qRes.reply.state,
                        msg: merge(qRes.reply.msg, pRes.reply.msg)
                    }
                }
            }

            return {
                consumed: pRes.consumed || qRes.consumed,
                reply: {
                    success: false,
                    msg: merge(qRes.reply.msg, pRes.reply.msg)
                }
            }
        } else {
            return failedMsg(pRes.reply.msg);
        }
    }
}

// (<*>) from Applicative
export function ap<A, B>(pf: Parser<(a: A) => B>, p: Parser<A>): Parser<B> {
    return (s: State) => {
        let pfRes = pf(s);
        if (pfRes.reply.success) {
            let pRes = p(pfRes.reply.state);
            if (pRes.reply.success) {
                return {
                    consumed: pfRes.consumed || pRes.consumed,
                    reply: {
                        success: true,
                        tree: pfRes.reply.tree(pRes.reply.tree),
                        state: pRes.reply.state,
                        msg: merge(pRes.reply.msg, pfRes.reply.msg)
                    }
                }
            }

            return {
                consumed: pfRes.consumed || pRes.consumed,
                reply: {
                    success: false,
                    msg: merge(pRes.reply.msg, pfRes.reply.msg)
                }
            }
        } else {
            return failedMsg(pfRes.reply.msg);
        }
    }
}

// (<**>) derived from Applicative
export function pa<A, B>(p: Parser<A>, pf: Parser<(a: A) => B>): Parser<B> {
    // return ap(fmap(a => f => f(a), p), pf)
    return (s: State) => {
        let pRes = p(s);
        if (pRes.reply.success) {
            let pfRes = pf(pRes.reply.state);
            if (pfRes.reply.success) {
                return {
                    consumed: pRes.consumed || pfRes.consumed,
                    reply: {
                        success: true,
                        tree: pfRes.reply.tree(pRes.reply.tree),
                        state: pfRes.reply.state,
                        msg: merge(pfRes.reply.msg, pRes.reply.msg)
                    }
                }
            }

            return {
                consumed: pRes.consumed || pfRes.consumed,
                reply: {
                    success: false,
                    msg: merge(pfRes.reply.msg, pRes.reply.msg)
                }
            }
        } else {
            return failedMsg(pRes.reply.msg);
        }
    }
}

// (<*) derived from Applicative
export function apL<A, B>(p: Parser<A>, q: Parser<B>): Parser<A> {
    // return ap(fmap(_const, p), q);
    return (s: State) => {
        let pRes = p(s);
        if (pRes.reply.success) {
            let qRes = q(pRes.reply.state);
            if (qRes.reply.success) {
                return {
                    consumed: pRes.consumed || qRes.consumed,
                    reply: {
                        success: true,
                        tree: pRes.reply.tree,
                        state: qRes.reply.state,
                        msg: merge(pRes.reply.msg, qRes.reply.msg)
                    }
                }
            }

            return {
                consumed: pRes.consumed || qRes.consumed,
                reply: {
                    success: false,
                    msg: merge(qRes.reply.msg, pRes.reply.msg)
                }
            }
        } else {
            return failedMsg(pRes.reply.msg);
        }
    }
}

// (*>) derived from Applicative
export function apR<A, B>(p: Parser<A>, q: Parser<B>): Parser<B> {
    // return ap(<Parser<(b: B) => B>>constFmapL(id, p), q);
    return (s: State) => {
        let pRes = p(s);
        if (pRes.reply.success) {
            let qRes = q(pRes.reply.state);
            if (qRes.reply.success) {
                return {
                    consumed: pRes.consumed || qRes.consumed,
                    reply: {
                        success: true,
                        tree: qRes.reply.tree,
                        state: qRes.reply.state,
                        msg: merge(pRes.reply.msg, qRes.reply.msg)
                    }
                }
            }

            return {
                consumed: pRes.consumed || qRes.consumed,
                reply: {
                    success: false,
                    msg: merge(qRes.reply.msg, pRes.reply.msg)
                }
            }
        } else {
            return failedMsg(pRes.reply.msg);
        }
    }
}

// (<|>) from Alternative
export function choice<A>(p: Parser<A>, q: Parser<A>): Parser<A> {
    return (s: State) => {
        let pRes = p(s);
        if (pRes.consumed) {
            return pRes;
        } else {
            let qRes = q(s);
            if (qRes.consumed) {
                return qRes;
            }

            if (pRes.reply.success) { // Empty ok
                return mergeOk(pRes.reply.tree, pRes.reply.state, pRes.reply.msg, qRes.reply.msg);
            } else {
                if (qRes.reply.success) {
                    return mergeOk(qRes.reply.tree, qRes.reply.state, pRes.reply.msg, qRes.reply.msg);
                } else {
                    return mergeErr(pRes.reply.msg, qRes.reply.msg);
                }
            }
        }
    }
}

export function many1<A>(p: Parser<A>): Parser<A[]> {
    return bind(p, x => bind(many(p), xs => pure([x, ...xs])))
}

export function many<A>(p: Parser<A>): Parser<A[]> {
    // return choice(many1(p), pure([]));
    return (s: State) => {
        let acc: A[] = [];
        let consumed: boolean = false;
        let r: Result<A> | undefined = undefined;
        let _s: State = s;
        let msg: Message | undefined = undefined;

        while (true) {
            r = p(_s);
            msg = (msg == undefined) ? r.reply.msg : merge(r.reply.msg, msg);

            if (r.reply.success) {
                consumed ||= r.consumed;
                _s = r.reply.state;
                acc.push(r.reply.tree);
            } else {
                return {
                    consumed: consumed,
                    reply: {
                        success: true,
                        tree: acc,
                        state: _s,
                        msg: msg
                    }
                }
            }
        }
    }
}

// (<~>) from Monoidal
export function mult<A, B>(p: Parser<A>, q: Parser<B>): Parser<[A, B]> {
    return (s: State) => {
        let pRes = p(s);
        if (pRes.reply.success) {
            let qRes = q(pRes.reply.state);
            if (qRes.reply.success) {
                return {
                    consumed: pRes.consumed || qRes.consumed,
                    reply: {
                        success: true,
                        tree: [pRes.reply.tree, qRes.reply.tree],
                        state: qRes.reply.state,
                        msg: qRes.reply.msg
                    }
                }
            }

            return {
                consumed: pRes.consumed || qRes.consumed,
                reply: {
                    success: false,
                    msg: merge(pRes.reply.msg, qRes.reply.msg)
                }
            }
        }
        return failedMsg(pRes.reply.msg);
    }
}

// (<~) or (<*) (done in terms of Monoidal)
export function multL<A, B>(p: Parser<A>, q: Parser<B>): Parser<A> {
    // return fmap((pair: [A, B]) => pair[0], mult(p, q));
    return apL(p, q);
}

// (~>) or (*>) (done in terms of Monoidal)
export function multR<A, B>(p: Parser<A>, q: Parser<B>): Parser<B> {
    // return fmap((pair: [A, B]) => pair[1], mult(p, q));
    return apR(p, q)
}

// (<$>) from Functor
export function fmap<A, B>(f: (a: A) => B, p: Parser<A>): Parser<B> {
    return (s: State) => {
        let pRes = p(s);
        return {
            consumed: pRes.consumed,
            reply: pRes.reply.success ? {...(pRes.reply), tree: f(pRes.reply.tree)} : <Err>pRes.reply
        }
    }
}

export function pamf<A, B>(p: Parser<A>, f: (a: A) => B): Parser<B> {
    return (s: State) => {
        let pRes = p(s);
        return {
            consumed: pRes.consumed,
            reply: pRes.reply.success ? {...(pRes.reply), tree: f(pRes.reply.tree)} : <Err>pRes.reply
        }
    }
}

// (<$) derived from Functor
export function constFmapL<A, B>(x: A, p: Parser<B>): Parser<A> {
    return (s: State) => {
        let pRes = p(s);
        return {
            consumed: pRes.consumed,
            reply: pRes.reply.success ? {...(pRes.reply), tree: x} : <Err>pRes.reply
        }
    }
}

// ($>)
export function constFmapR<A, B>(p: Parser<B>, x: A): Parser<A> {
    return (s: State) => {
        let pRes = p(s);
        return {
            consumed: pRes.consumed,
            reply: pRes.reply.success ? {...(pRes.reply), tree: x} : <Err>pRes.reply
        }
    }
}

export function attempt<A>(p: Parser<A>): Parser<A> {
    return (s: State) => {
        let pRes = p(s);
        if (pRes.consumed && !pRes.reply.success) {
            return failedMsg(pRes.reply.msg);
        }
        return pRes;
    }
}

// note that (x: A) => (xs: A[]) => [x, ...xs] is equivalent to (:)
// (<:>) derived from Applicative
export function liftCons<A>(px: Parser<A>, pxs: Parser<A[]>): Parser<A[]> {
    return ap(fmap((x: A) => (xs: A[]) => [x, ...xs], px), pxs);
    // return ap(fmap(curryCons, px), pxs);
}

// shorthand for (l *> p <* r)
export function between<A, B, C>(l: Parser<A>, p: Parser<B>, r: Parser<C>): Parser<B> {
    return multR(l, multL(p, r));
}

// (<?>) for error messages
export function label<A>(p: Parser<A>, exp: string): Parser<A> {
    function generateMsg(msg: Message): Message {
        return {...msg, expected: [exp]};
    }

    return (s: State) => {
        let pRes = p(s);
        if (pRes.consumed) {
            return pRes;
        } else {
            return {
                consumed: pRes.consumed,
                reply: {...(pRes.reply), msg: generateMsg(pRes.reply.msg)}
            }
        }
    }
}

// attempts and also consumes literal spaces
export function token<A>(p: Parser<A>): Parser<A> {
    return multL(attempt(p), many(chr(" ")));
}

export function cT(c: string): Parser<string> {
    return token(chr(c));
}

export function chainl1<A>(p: Parser<A>, op: Parser<(a1: A, a2: A) => A>): Parser<A> {
    return (s: State) => {
        let acc: A;
        let consumed: boolean = false;
        let r: Result<A>;
        let _s: State = s;
        let msg: Message;

        r = p(_s);
        if (r.reply.success) {
            msg = r.reply.msg;
            _s = r.reply.state;
            acc = r.reply.tree;

            while (true) {
                let opr = op(_s);
                msg = merge(opr.reply.msg, msg);

                if (opr.reply.success) {
                    consumed ||= opr.consumed;
                    _s = opr.reply.state;
                    msg = merge(opr.reply.msg, msg);

                    let oprf = opr.reply.tree;
                    let qr = p(_s);
                    msg = merge(qr.reply.msg, msg)
                    if (qr.reply.success) {
                        consumed ||= qr.consumed;
                        _s = qr.reply.state;

                        acc = oprf(acc, qr.reply.tree);
                    } else {
                        return {
                            consumed: consumed,
                            reply: {
                                success: false,
                                msg: msg
                            }
                        }
                    }
                } else {
                    return {
                        consumed: consumed,
                        reply: {
                            success: true,
                            tree: acc,
                            state: _s,
                            msg: msg
                        }
                    }
                }
            }
        } else {
            return r;
        }
    }
}

export function chainr1<A>(p: Parser<A>, op: Parser<(a1: A, a2: A) => A>): Parser<A> {
    function reduce(ps: A[], ops: ((a1: A, a2: A) => A)[]): A {
        let acc: A = ps[ps.length - 1];
        let n = ops.length;
        for (let i = 0; i < n; i++) {
            acc = ops[n - i - 1](ps[n - i - 1], acc)
        }
        return acc;
    }

    return (s: State) => {
        let ps: A[] = [];
        let ops: ((a1: A, a2: A) => A)[] = []
        let consumed: boolean = false;
        let r: Result<A>;
        let _s: State = s;
        let msg: Message;

        r = p(_s);
        if (r.reply.success) {
            msg = r.reply.msg;
            _s = r.reply.state;
            ps.push(r.reply.tree);

            while (true) {
                let opr = op(_s);
                msg = merge(opr.reply.msg, msg);

                if (opr.reply.success) {
                    consumed ||= opr.consumed;
                    _s = opr.reply.state;
                    msg = merge(opr.reply.msg, msg);

                    ops.push(opr.reply.tree);
                    let qr = p(_s);
                    msg = merge(qr.reply.msg, msg)
                    if (qr.reply.success) {
                        consumed ||= qr.consumed;
                        _s = qr.reply.state;

                        ps.push(qr.reply.tree)
                    } else {
                        return {
                            consumed: consumed,
                            reply: {
                                success: false,
                                msg: msg
                            }
                        }
                    }
                } else {
                    return {
                        consumed: consumed,
                        reply: {
                            success: true,
                            tree: reduce(ps, ops),
                            state: _s,
                            msg: msg
                        }
                    }
                }
            }
        } else {
            return r;
        }
    }
}

export function postfix<A>(p: Parser<A>, op: Parser<(a: A) => A>): Parser<A> {
    return (s: State) => {
        let acc: A;
        let consumed: boolean = false;
        let r: Result<A>;
        let _s: State = s;
        let msg: Message;

        r = p(_s)
        if (r.reply.success) {
            msg = r.reply.msg;
            _s = r.reply.state;
            acc = r.reply.tree;

            while (true) {
                let opr = op(_s);
                msg = merge(opr.reply.msg, msg);

                if (opr.reply.success) {
                    consumed ||= opr.consumed;
                    _s = opr.reply.state;
                    acc = opr.reply.tree(acc);
                } else {
                    return {
                        consumed: consumed,
                        reply: {
                            success: true,
                            tree: acc,
                            state: _s,
                            msg: msg
                        }
                    }
                }
            }
        } else {
            return r;
        }
    }
}

export function lift2<A, B, R>(_f: (a: A, b: B) => R, p: Parser<A>, q: Parser<B>): Parser<R> {
    return ap(fmap((a: A) => (b: B) => _f(a, b), p), q);
}

export function lift3<A, B, C, R>(_f: (a: A, b: B, c: C) => R, p: Parser<A>, q: Parser<B>, r: Parser<C>): Parser<R> {
    return ap(lift2((a: A, b: B) => (c: C) => _f(a, b, c), p, q), r);
}

export function lift4<A, B, C, D, R>(_f: (a: A, b: B, c: C, d: D) => R, p: Parser<A>, q: Parser<B>, r: Parser<C>, s: Parser<D>): Parser<R> {
    return ap(lift3((a: A, b: B, c: C) => (d: D) => _f(a, b, c, d), p, q, r), s);
}

export function lift5<A, B, C, D, E, R>(_f: (a: A, b: B, c: C, d: D, e: E) => R, p: Parser<A>, q: Parser<B>, r: Parser<C>, s: Parser<D>, t: Parser<E>): Parser<R> {
    return ap(lift4((a: A, b: B, c: C, d: D) => (e: E) => _f(a, b, c, d, e), p, q, r, s), t);
}

export function lift6<A, B, C, D, E, F, R>(_f: (a: A, b: B, c: C, d: D, e: E, f: F) => R, p: Parser<A>, q: Parser<B>, r: Parser<C>, s: Parser<D>, t: Parser<E>, u: Parser<F>): Parser<R> {
    return ap(lift5((a: A, b: B, c: C, d: D, e: E) => (f: F) => _f(a, b, c, d, e, f), p, q, r, s, t), u);
}

export function lift7<A, B, C, D, E, F, G, R>(_f: (a: A, b: B, c: C, d: D, e: E, f: F, g: G) => R, p: Parser<A>, q: Parser<B>, r: Parser<C>, s: Parser<D>, t: Parser<E>, u: Parser<F>, v: Parser<G>): Parser<R> {
    return ap(lift6((a: A, b: B, c: C, d: D, e: E, f: F) => (g: G) => _f(a, b, c, d, e, f, g), p, q, r, s, t, u), v);
}

export function lift8<A, B, C, D, E, F, G, H, R>(_f: (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H) => R, p: Parser<A>, q: Parser<B>, r: Parser<C>, s: Parser<D>, t: Parser<E>, u: Parser<F>, v: Parser<G>, w: Parser<H>): Parser<R> {
    return ap(lift7((a: A, b: B, c: C, d: D, e: E, f: F, g: G) => (h: H) => _f(a, b, c, d, e, f, g, h), p, q, r, s, t, u, v), w);
}

export function lift9<A, B, C, D, E, F, G, H, I, R>(_f: (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I) => R, p: Parser<A>, q: Parser<B>, r: Parser<C>, s: Parser<D>, t: Parser<E>, u: Parser<F>, v: Parser<G>, w: Parser<H>, x: Parser<I>): Parser<R> {
    return ap(lift8((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H) => (i: I) => _f(a, b, c, d, e, f, g, h, i), p, q, r, s, t, u, v, w), x);
}

export function curry<A, B, C>(f: (a: A, b: B) => C): (a: A) => (b: B) => C {
    return (a: A) => (b: B) => f(a, b);
}

export function uncurry<A, B, C>(f: (a: A) => (b: B) => C): (a: A, b: B) => C {
    return (a: A, b: B) => f(a)(b);
}

// g is the function applied to the first argument
export function mapFirstArg<A, B, C, D>(g: (d: D) => A, f: (a: A, b: B) => C): (d: D, b: B) => C {
    return (d: D, b: B) => f(g(d), b);
}

export function unpairFirstArg<A, B, C, D>(f: (ab: [A, B], c: C) => D): (a: A, bc: [B, C]) => D {
    return (a: A, bc: [B, C]) => f([a, bc[0]], bc[1]);
}

export function paTransform<A, B, C, D>(f: (b: B, c: C) => D): (a2b: (a: A) => B, c: C) => (a: A) => D {
    return (a2b: (a0: A) => B, c: C) => (a: A) => f(a2b(a), c);
}

export function apFromMult<A, B>([a2b, a]: [(a0: A) => B, A]): B {
    return a2b(a);
}

export function paFromMult<A, B>([a, a2b]: [A, (a0: A) => B]): B {
    return a2b(a);
}

export function argsToTuple<A, B, C>(f: (ab: [A, B]) => C): (a: A, b: B) => C {
    return (a: A, b: B) => f([a, b]);
}

export function lsc(p: Parser<string>, q: Parser<string>): Parser<string> {
    return lift2((a: string, b: string) => a + b, p, q);
}

export let se: Parser<string> = pure("");