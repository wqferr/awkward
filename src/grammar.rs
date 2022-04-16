use chumsky::prelude::*;
use std::collections::HashMap;

use super::record::Record;
use super::types::{Value, Number, BuiltinFunction};

pub struct EvaluationContext<'a> {
    current_record: &'a Record,
    ofs: &'a str,
    field_names: HashMap<String, usize>,

    functions: HashMap<String, BuiltinFunction>
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    // These reference field index with their values
    StrVar(usize),
    NumVar(usize),
    // BoolVar?

    StrNamedVar(String),
    NumNamedVar(String),
    // BoolNamedVar?

    StrLiteral(String),
    NumLiteral(Number),
    // BoolLiteral?

    FnCall(String, Vec<Expr>),

    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Concat(Box<Expr>, Box<Expr>),

    Eq(Box<Expr>, Box<Expr>),
    Ineq(Box<Expr>, Box<Expr>),
    Greater(Box<Expr>, Box<Expr>),
    GreaterEq(Box<Expr>, Box<Expr>),
    Lesser(Box<Expr>, Box<Expr>),
    LesserEq(Box<Expr>, Box<Expr>),

    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
}

pub fn eval(expr: &Expr, ctx: &EvaluationContext) -> Value {
    use Expr::*;
    match expr {
        StrVar(idx) => {
            if *idx == 0 {
                let mut buf = Box::new(Vec::new());
                ctx.current_record.write(&mut buf, ctx.ofs).unwrap();
                Value::Str(String::from_utf8(*buf).unwrap())
            } else {
                Value::Str(ctx.current_record.nth_str(*idx).unwrap_or("").to_owned())
            }
        },
        NumVar(idx) => Value::Num(ctx.current_record.nth_num(*idx).unwrap().to_owned()),

        StrNamedVar(name) => {
            let idx = ctx.field_names[name];
            Value::Str(ctx.current_record.nth_str(idx).unwrap().to_owned())
        }
        NumNamedVar(name) => {
            let idx = ctx.field_names[name];
            Value::Num(ctx.current_record.nth_num(idx).unwrap())
        }

        StrLiteral(s) => Value::Str(s.clone()),
        NumLiteral(n) => Value::Num(n.clone()),

        Neg(x) => Value::Num(-eval_num(x, ctx)),
        Add(x, y) => Value::Num(eval_num(x, ctx) + eval_num(y, ctx)),
        Sub(x, y) => Value::Num(eval_num(x, ctx) - eval_num(y, ctx)),
        Mul(x, y) => Value::Num(eval_num(x, ctx) * eval_num(y, ctx)),
        Div(x, y) => Value::Num(eval_num(x, ctx) / eval_num(y, ctx)),
        Mod(x, y) => Value::Num(eval_num(x, ctx) % eval_num(y, ctx)),
        Concat(x, y) => {
            let mut s1 = eval_str(x, ctx);
            let s2 = eval_str(y, ctx);
            s1.push_str(s2.as_str());

            Value::Str(s1)
        },

        Eq(x, y) => Value::Bool(eval(x, ctx) == eval(y, ctx)),
        Ineq(x, y) => Value::Bool(eval(x, ctx) != eval(y, ctx)),
        Greater(x, y) => Value::Bool(eval_num(x, ctx) > eval_num(y, ctx)),
        GreaterEq(x, y) => Value::Bool(eval_num(x, ctx) >= eval_num(y, ctx)),
        Lesser(x, y) => Value::Bool(eval_num(x, ctx) < eval_num(y, ctx)),
        LesserEq(x, y) => Value::Bool(eval_num(x, ctx) <= eval_num(y, ctx)),

        And(x, y) => Value::Bool(eval_bool(x, ctx) && eval_bool(y, ctx)),
        Or(x, y) => Value::Bool(eval_bool(x, ctx) || eval_bool(y, ctx)),
        FnCall(fname, args) => {
            let f = &ctx.functions[fname];
            let args = args.iter().map(|e| eval(e, ctx)).collect();
            f(args)
        },
    }
}

pub fn eval_num(expr: &Expr, ctx: &EvaluationContext) -> Number {
    match eval(expr, ctx) {
        Value::Num(x) => x,
        Value::Str(s) => panic!("Expected number, found string `{}`", s),
        Value::Bool(b) => panic!("Expected number, found bool `{}`", b)
    }
}

pub fn eval_bool(expr: &Expr, ctx: &EvaluationContext) -> bool {
    match eval(expr, ctx) {
        Value::Bool(b) => b,
        Value::Str(s) => panic!("Expected bool, found string `{}`", s),
        Value::Num(x) => panic!("Expected bool, found number `{}`", x)
    }
}

pub fn eval_str(expr: &Expr, ctx: &EvaluationContext) -> String {
    match eval(expr, ctx) {
        Value::Str(s) => s,
        Value::Bool(b) => if b {"true".to_owned()} else {"false".to_owned()},
        Value::Num(x) => format!("{}", x)
    }
}

pub fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    // This grammar was adapted from Chumsky's tutorial

    recursive(|expr| {
        // number literal
        let num = text::int(10)
            .then(
                just('.')
                .ignore_then(text::int(10))
                .or_not()
            )
            .map(|(s1, s2)| {
                let s2 = s2.unwrap_or_else(|| String::from("0"));
                Expr::NumLiteral([s1, s2].join(".").parse().unwrap())
            })
            .padded();

        // escaped character
        let escape =
            just('\\').ignore_then(
                just('\\')
                .or(just('"'))
                .or(just('\''))
                .or(just('n').to('\n'))
                .or(just('r').to('\r'))
                .or(just('t').to('\t')));

        let quoted_str =
            |delim: char|
                just(delim)
                    .ignore_then(filter(move |c| *c != '\\' && *c != delim).or(escape).repeated())
                    .then_ignore(just(delim))
                    .map(|v| Expr::StrLiteral(v.into_iter().collect::<String>()));

        let string_dq = quoted_str('"');
        let string_sq = quoted_str('\'');
        
        let call =
            text::ident().then(
                expr.clone()
                    .separated_by(just(','))
                    .allow_trailing()
                    .delimited_by(just('('), just(')'))
            )
            .map(|(f, args)| Expr::FnCall(f, args));

        // starting characters for variable referencing
        // i.e., either n@ or just @
        let var_start = just('n')
            .or_not()
            .then_ignore(just('@'));

        // single term for arithmetic expressions
        let atom = num
            .or(expr.delimited_by(just('('), just(')')))
            .or(
                var_start.clone()
                .then(text::int(10))
                .map(|(t, x)| {
                    if t.is_some() {
                        Expr::NumVar(x.parse().unwrap())
                    } else {
                        Expr::StrVar(x.parse().unwrap())
                    }
                })
            )
            .or(
                var_start.clone()
                .then(text::ident())
                .map(|(t, name)| {
                    if t.is_some() {
                        Expr::NumNamedVar(name)
                    } else {
                        Expr::StrNamedVar(name)
                    }
                })
            )
            .or(call)
            .or(string_dq)
            .or(string_sq)
            .padded();
    
        // operators with a single char
        let op = |c| just(c).padded();
    
        // operators with 2 chars
        let double_char_op = |c1, c2| just(c1).then(just(c2)).padded();
    
        // unary minus
        let unary = op('-')
            .repeated()
            .then(atom)
            .foldr(|_op, rhs| Expr::Neg(Box::new(rhs)));
    
        // product / division / mod
        let product = unary.clone()
            .then(
                op('*').to(Expr::Mul as fn(_, _) -> _)
                .or(op('/').to(Expr::Div as fn(_, _) -> _))
                .or(op('%').to(Expr::Mod as fn(_, _) -> _))
                .then(unary)
                .repeated()
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));
    
        // add / subtract
        let sum = product.clone()
            .then(
                op('+').to(Expr::Add as fn(_, _) -> _)
                .or(op('-').to(Expr::Sub as fn(_, _) -> _))
                .or(double_char_op('.', '.').to(Expr::Concat as fn(_, _) -> _))
                .then(product)
                .repeated()
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        // relationals
        let comparison = sum.clone()
            .then(
                op('>').to(Expr::Greater as fn(_, _) -> _)
                .or(double_char_op('>', '=').to(Expr::GreaterEq as fn(_, _) -> _))
                .or(op('<').to(Expr::Lesser as fn(_, _) -> _))
                .or(double_char_op('<', '=').to(Expr::LesserEq as fn(_, _) -> _))
                .or(double_char_op('=', '=').to(Expr::Eq as fn(_, _) -> _))
                .or(double_char_op('!', '=').to(Expr::Ineq as fn(_, _) -> _))
                .then(sum)
                .repeated()
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        // boolean and
        let and = comparison.clone()
            .then(
                double_char_op('&', '&')
                .then(comparison)
                .repeated()
            )
            .foldl(|lhs, (_op, rhs)| Expr::And(Box::new(lhs), Box::new(rhs)));

        // boolean or
        let or = and.clone()
            .then(
                double_char_op('|', '|')
                .then(and)
                .repeated()
            )
            .foldl(|lhs, (_op, rhs)| Expr::Or(Box::new(lhs), Box::new(rhs)));

        or
    })
    .then_ignore(end())
}

#[cfg(test)]
mod test {
    use super::*;

    fn simple_eval(expr: &str) -> Value {
        let p = parser();
        let r = Record::from(vec![
            "abc".to_owned(),
            "2".to_owned(),
            "lol".to_owned()
        ]);
        let c = |args: Vec<Value>| {
            if let Value::Num(x) = args[0] {
                Value::Num(2*x)
            } else {
                // FIXME maybe make builtins return a result instead of straight up panicking
                panic!("expected number, found {}", args[0]);
            }
        };
        let ctx = EvaluationContext {
            current_record: &r,
            ofs: ",",
            field_names: HashMap::from([
                ("first".to_owned(), 1),
                ("second".to_owned(), 2),
                ("third".to_owned(), 3)
            ]),
            functions: HashMap::from([
                ("double".to_owned(), Box::new(c) as BuiltinFunction)
            ])
        };

        let res = p.parse(expr);
        assert!(res.is_ok());
        eval(&res.unwrap(), &ctx)
    }

    #[test]
    fn test_number() {
        assert_eq!(simple_eval("1"), Value::Num(1i32.into()));
        assert_eq!(simple_eval("0.51"), Value::Num(Number::from_num(0.51)));
    }

    #[test]
    fn test_arithmetic() {
        assert_eq!(simple_eval("1 + (4/2 - 1)"), Value::Num(2.into()));
        assert_eq!(simple_eval("10 % 3"), Value::Num(1.into()));
    }

    #[test]
    fn test_boolean() {
        assert_eq!(simple_eval("1 > 2"), Value::Bool(false));
        assert_eq!(simple_eval("1 + 1 == 2"), Value::Bool(true));
        assert_eq!(simple_eval("0.5 + 2 + 1.74 == 4.24"), Value::Bool(true));
    }

    #[test]
    fn test_fields() {
        assert_eq!(simple_eval("@1"), Value::Str("abc".to_owned()));
        assert_eq!(simple_eval("@2"), Value::Str("2".to_owned()));
        assert_eq!(simple_eval("n@2"), Value::Num(2.into()));
        assert_eq!(simple_eval("@3"), Value::Str("lol".to_owned()));

        assert_eq!(simple_eval("@first"), Value::Str("abc".to_owned()));
        assert_eq!(simple_eval("@second"), Value::Str("2".to_owned()));
        assert_eq!(simple_eval("n@second"), Value::Num(2.into()));
        assert_eq!(simple_eval("@third"), Value::Str("lol".to_owned()));

        assert_eq!(simple_eval("@0"), Value::Str("abc,2,lol".to_owned()));
    }

    #[test]
    fn test_padding() {
        assert_eq!(simple_eval(" ( n@2+       1 )  /     2"), Value::Num(Number::from_num(1.5)));
    }

    #[test]
    fn test_str_literals() {
        assert_eq!(simple_eval("'th\"is is \\'a string'"), Value::Str("th\"is is 'a string".to_owned()));
        assert_eq!(simple_eval("\"double \\\"quote' test\""), Value::Str("double \"quote' test".to_owned()));
    }

    #[test]
    fn test_str_concatenation() {
        assert_eq!(simple_eval("'this' .. (1>2)..'is'.. (\"a\" ..1) .. 'test'"), Value::Str("thisfalseisa1test".to_owned()));
    }

    #[test]
    fn test_function_calls() {
        assert_eq!(simple_eval("double(4)"), Value::Num(8.into()));
    }
}