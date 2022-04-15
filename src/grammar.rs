use chumsky::prelude::*;

use super::record::Record;
use super::value::{Value, Number};

pub struct EvaluationContext<'a> {
    current_record: &'a Record,
    ofs: &'a str,
    // field_names: Vec<String>
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    // These reference field index with their values
    StrVar(usize),
    NumVar(usize),
    // BoolVar?

    StrLiteral(String),
    NumLiteral(Number),
    // BoolLiteral?

    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),

    Eq(Box<Expr>, Box<Expr>),
    Ineq(Box<Expr>, Box<Expr>),
    Greater(Box<Expr>, Box<Expr>),
    GreaterEq(Box<Expr>, Box<Expr>),
    Lesser(Box<Expr>, Box<Expr>),
    LesserEq(Box<Expr>, Box<Expr>),

    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
}

const STRING_ARITHMETIC_PANIC: &str = "Cannot perform arithmetic on strings";

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

        StrLiteral(s) => Value::Str(s.clone()),
        NumLiteral(n) => Value::Num(n.clone()),

        Neg(x) => Value::Num(-eval_num(x, ctx)),
        Add(x, y) => Value::Num(eval_num(x, ctx) + eval_num(y, ctx)),
        Sub(x, y) => Value::Num(eval_num(x, ctx) - eval_num(y, ctx)),
        Mul(x, y) => Value::Num(eval_num(x, ctx) * eval_num(y, ctx)),
        Div(x, y) => Value::Num(eval_num(x, ctx) / eval_num(y, ctx)),
        Mod(x, y) => Value::Num(eval_num(x, ctx) % eval_num(y, ctx)),

        Eq(x, y) => Value::Bool(eval(x, ctx) == eval(y, ctx)),
        Ineq(x, y) => Value::Bool(eval(x, ctx) != eval(y, ctx)),
        Greater(x, y) => Value::Bool(eval_num(x, ctx) > eval_num(y, ctx)),
        GreaterEq(x, y) => Value::Bool(eval_num(x, ctx) >= eval_num(y, ctx)),
        Lesser(x, y) => Value::Bool(eval_num(x, ctx) < eval_num(y, ctx)),
        LesserEq(x, y) => Value::Bool(eval_num(x, ctx) <= eval_num(y, ctx)),

        And(x, y) => Value::Bool(eval_bool(x, ctx) && eval_bool(y, ctx)),
        Or(x, y) => Value::Bool(eval_bool(x, ctx) || eval_bool(y, ctx)),

        _ => panic!("Unknown token!")
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

pub fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    // This grammar was adapted from Chumsky's tutorial

    recursive(|expr| {
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
    
        let atom = num
            .or(expr.delimited_by(just('('), just(')')))
            .or(
                just('@')
                .ignore_then(text::int(10))
                .map(|x| Expr::StrVar(x.parse().unwrap()))
            );
    
        let op = |c| just(c).padded();
    
        let unary = op('-')
            .repeated()
            .then(atom)
            .foldr(|_op, rhs| Expr::Neg(Box::new(rhs)));
    
        // TODO remainder
        let product = unary.clone()
            .then(
                op('*').to(Expr::Mul as fn(_, _) -> _)
                .or(op('/').to(Expr::Div as fn(_, _) -> _))
                .or(op('%').to(Expr::Mod as fn(_, _) -> _))
                .then(unary)
                .repeated()
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));
    
        let sum = product.clone()
            .then(
                op('+').to(Expr::Add as fn(_, _) -> _)
                .or(op('-').to(Expr::Sub as fn(_, _) -> _))
                .then(product)
                .repeated()
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));
    
        let double_char_op = |c1, c2| just(c1).then(just(c2)).padded();

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

        let and = comparison.clone()
            .then(
                double_char_op('&', '&')
                .then(comparison)
                .repeated()
            )
            .foldl(|lhs, (_op, rhs)| Expr::And(Box::new(lhs), Box::new(rhs)));

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
            "first".to_owned(),
            "second".to_owned(),
            "third".to_owned()
        ]);
        let ctx = EvaluationContext {
            current_record: &r,
            ofs: ","
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
        assert_eq!(simple_eval("1 < 2"), Value::Bool(true));
        assert_eq!(simple_eval("1 + 1 == 2"), Value::Bool(true));
    }

    #[test]
    fn test_fields() {
        assert_eq!(simple_eval("@1"), Value::Str("first".to_owned()));
        assert_eq!(simple_eval("@2"), Value::Str("second".to_owned()));
        assert_eq!(simple_eval("@3"), Value::Str("third".to_owned()));

        assert_eq!(simple_eval("@0"), Value::Str("first,second,third".to_owned()));
    }
}