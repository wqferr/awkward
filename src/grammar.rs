use chumsky::prelude::*;
use regex::Regex;

use crate::expr::{Expr, Declaration, FieldId};


fn expr_parser() -> impl Parser<char, Expr, Error=Simple<char>> + Clone {
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
                choice((
                    just('\\'),
                    just('/'),
                    just('"'),
                    just('\''),
                    just('n').to('\n'),
                    just('r').to('\r'),
                    just('t').to('\t')
                ))
            );
        let bool_lit =
            text::keyword("true").to(true)
            .or(text::keyword("false").to(false))
            .map(Expr::BoolLiteral);

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

        let var = 
            just('n')
            .or_not()
            .then_ignore(just('@'))
            .then(
                text::int(10).map(|x: String| FieldId::Idx(x.parse::<usize>().unwrap()))
                .or(text::ident().map(|n| FieldId::Name(n)))
            )
            .map(|(t, id)| {
                match t {
                    Some('n') => Expr::NumVar(id),
                    _ => Expr::StrVar(id)
                }
            });

        let start = text::keyword("start").to(Expr::Start);
        let end = text::keyword("end").to(Expr::End);
        let regex = 
            just('/')
            .ignore_then(
                filter(move |c| *c != '\\' && *c != '/')
                .or(
                    just('\\')
                    .ignore_then(just('/'))
                )
                .repeated())
            .then_ignore(just('/'))
            .then(
                just('@')
                .ignore_then(
                    text::int(10)
                    .or(text::ident())
                )
                .or_not()
            )
            .map(
                |(v, field)| {
                    let field_id = match field {
                        Some(field) => match field.parse::<usize>() {
                            Ok(field_idx) => FieldId::Idx(field_idx),
                            Err(_) => FieldId::Name(field),
                        },
                        None => FieldId::Idx(0)
                    };
                    Expr::RegexSearch {
                        re: Regex::new(
                            v.into_iter().collect::<String>().as_str()
                        ).unwrap(),
                        field: field_id
                    }
                }
            );

        // single term for arithmetic expressions
        let atom =
            choice((
                num,
                expr.delimited_by(just('('), just(')')),
                var,
                call,
                bool_lit,
                string_dq,
                string_sq,
                start,
                end,
                regex
            )).padded();
    
        // operators with a single char
        let op = |c| just(c).padded();
    
        // operators with 2 chars
        let double_char_op = |c1, c2| just(c1).then(just(c2)).padded();
    
        // unary minus
        let unary =
            op('-').to(Expr::Neg as fn(_) -> _)
            .or(op('!').to(Expr::Not as fn(_) -> _))
            .repeated()
            .then(atom)
            .foldr(|op, rhs| op(Box::new(rhs)));
    
        // product / division / mod
        let product = unary.clone()
            .then(
                choice((
                    op('*').to(Expr::Mul as fn(_, _) -> _),
                    op('/').to(Expr::Div as fn(_, _) -> _),
                    op('%').to(Expr::Mod as fn(_, _) -> _)
                ))
                .then(unary)
                .repeated()
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));
    
        // add / subtract
        let sum = product.clone()
            .then(
                choice((
                    op('+').to(Expr::Add as fn(_, _) -> _),
                    op('-').to(Expr::Sub as fn(_, _) -> _),
                    double_char_op('.', '.').to(Expr::ConcatWSep as fn(_, _) -> _),
                    op('.').to(Expr::Concat as fn(_, _) -> _)
                ))
                .then(product)
                .repeated()
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        // relationals
        let comparison = sum.clone()
            .then(
                choice((
                    op('>').to(Expr::Greater as fn(_, _) -> _),
                    double_char_op('>', '=').to(Expr::GreaterEq as fn(_, _) -> _),
                    op('<').to(Expr::Lesser as fn(_, _) -> _),
                    double_char_op('<', '=').to(Expr::LesserEq as fn(_, _) -> _),
                    double_char_op('=', '=').to(Expr::Eq as fn(_, _) -> _),
                    double_char_op('!', '=').to(Expr::Ineq as fn(_, _) -> _),
                ))
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
}

pub fn program_parser() -> impl Parser<char, Vec<Declaration>, Error=Simple<char>> {
    // This grammar was adapted from Chumsky's tutorial

    let expr = expr_parser();
    let decl =
        expr.clone()
        .then_ignore(just("->"))
        .or_not()
        .then(
            expr.clone()
            .then(
                just(',')
                .ignore_then(expr)
                .repeated()
            )
        )
        .map(|(pat, (first, mut rest))| {
            let actions = {
                let mut a = vec![first];
                a.append(&mut rest);
                a
            };
            Declaration::new(
                // if pattern is omitted, execute for every record after it's started
                pat.unwrap_or(Expr::HasRecord),
                actions
            )
        });

    let program =
        decl.clone()
        .then(
            just(';').padded()
            .ignore_then(decl.or_not())
            .repeated()
        )
        .map(|(d, vd): (Declaration, Vec<Option<Declaration>>)| {
            let mut v = vec![d];
            for other in vd.into_iter() {
                if let Some(other) = other {
                    v.push(other);
                }
            }
            v
        });

    program.then_ignore(end())
}

#[cfg(test)]
mod test {
    use std::fmt::Write;

    use super::*;
    use crate::record::Record;
    use crate::types::{Number, Value};
    use crate::expr::EvaluationContext;

    fn simple_eval(expr: &str) -> Value {
        let r = Record::from("abc,2,lol".to_owned(), ",");
        let double = |args: Vec<Value>| {
            if let Value::Num(x) = args[0] {
                Value::Num(2*x)
            } else {
                // FIXME maybe make builtins return a result instead of straight up panicking
                panic!("expected number, found {}", args[0]);
            }
        };
        let mut ctx = EvaluationContext::new(",".to_owned());
        ctx.set_field_names(vec!["first".to_owned(), "second".to_owned(), "third".to_owned()]);
        ctx.register_builtin("double", double);
        ctx.set_current_record(r);

        let res = expr_parser().parse(expr);
        match res {
            Ok(expr) => expr.eval(&mut ctx),
            Err(e) => panic!("{:?}", e),
        }
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
        assert_eq!(simple_eval("!(1 > 2)"), Value::Bool(true));
        assert_eq!(simple_eval("1 + 1 == 2"), Value::Bool(true));
        assert_eq!(simple_eval("0.5 + 2 + 1.74 == 4.24"), Value::Bool(true));
        assert_eq!(simple_eval("true || false"), Value::Bool(true));
        assert_eq!(simple_eval("true && false"), Value::Bool(false));
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
        assert_eq!(
            simple_eval("'th\"is is \\'a string'"),
            Value::Str("th\"is is 'a string".to_owned())
        );
        assert_eq!(
            simple_eval("\"double \\\"quote' test\""),
            Value::Str("double \"quote' test".to_owned())
        );
    }

    #[test]
    fn test_str_concatenation() {
        assert_eq!(
            simple_eval("'this' . (1.5>2).'is'. (\"a\" .1) . 'test'"),
            Value::Str("thisfalseisa1test".to_owned())
        );
        assert_eq!(
            simple_eval("'this' .. ('is'.. 'another') . 'test'"),
            Value::Str("this,is,anothertest".to_owned())
        );
    }

    #[test]
    fn test_function_calls() {
        assert_eq!(simple_eval("double(4)"), Value::Num(8.into()));
        assert_eq!(simple_eval("double(double(3)) + double(2.5) + 2"), Value::Num(19.into()));
    }

    #[test]
    fn test_search() {
        assert_eq!(simple_eval("/lo/"), Value::Bool(true));
        assert_eq!(simple_eval("/^lo/"), Value::Bool(false));
        assert_eq!(simple_eval("/lo/@0"), Value::Bool(true));
        assert_eq!(simple_eval("/^lo/@0"), Value::Bool(false));
        assert_eq!(simple_eval("/lo/@2"), Value::Bool(false));
        assert_eq!(simple_eval("/lo/@3"), Value::Bool(true));
        assert_eq!(simple_eval("/^lo/@3"), Value::Bool(true));
        assert_eq!(simple_eval("/lo/@second"), Value::Bool(false));
        assert_eq!(simple_eval("/lo/@third"), Value::Bool(true));
        assert_eq!(simple_eval("/^lo/@third"), Value::Bool(true));
    }

    #[test]
    fn test_decl() {
        use std::cell::RefCell;
        use std::rc::Rc;

        let decl = program_parser().parse(r#"
            (nr() == 1) -> putline("start");
            put(@2), put(@1);
            (nr() % 2 == 0) -> put("thing!"), put("other thing!");
        "#).unwrap();
        let mut ctx = EvaluationContext::new(",".to_owned());

        let record_number_original = Rc::new(RefCell::new(0usize));
        let buf_original = Rc::new(RefCell::new(String::new()));
        let line_is_empty_original = Rc::new(RefCell::new(true));

        let buf = buf_original.clone();
        let line_is_empty = line_is_empty_original.clone();
        let put = move |vs: Vec<Value>| {
            for v in vs.iter() {
                if *line_is_empty.borrow() {
                    *line_is_empty.borrow_mut() = false;
                } else {
                    write!(buf.borrow_mut(), ",").unwrap();
                }
                write!(buf.borrow_mut(), "{}", v).unwrap();
            }
            Value::Bool(true)
        };
        ctx.register_builtin("put", put);

        let buf = buf_original.clone();
        let line_is_empty = line_is_empty_original.clone();
        let putline = move |vs: Vec<Value>| {
            for v in vs.iter() {
                if !*line_is_empty.borrow() {
                    write!(buf.borrow_mut(), "\n").unwrap();
                    *line_is_empty.borrow_mut() = true;
                }
                write!(buf.borrow_mut(), "{}\n", v).unwrap();
            }
            Value::Bool(true)
        };
        ctx.register_builtin("putline", putline);

        let record_number = record_number_original.clone();
        let nr = move |_vs: Vec<Value>| {
            Value::Num(Number::from_num(*record_number.borrow()))
        };
        ctx.register_builtin("nr", nr);

        (*record_number_original.borrow_mut()) += 1;
        (*line_is_empty_original.borrow_mut()) = true;
        ctx.set_current_record(Record::from("hello,there".to_owned(), ","));
        for d in decl.iter() {
            d.execute_if_applies(&mut ctx);
        }
        (*buf_original.borrow_mut()).write_char('\n').unwrap();

        (*record_number_original.borrow_mut()) += 1;
        (*line_is_empty_original.borrow_mut()) = true;
        ctx.set_current_record(Record::from("general,kenobi".to_owned(), ","));
        for d in decl.iter() {
            d.execute_if_applies(&mut ctx);
        }
        (*buf_original.borrow_mut()).write_char('\n').unwrap();

        // (*record_number_original.borrow_mut()) = 0;
        // (*line_is_empty_original.borrow_mut()) = true;
        // ctx.finish_parsing();

        assert_eq!(r#"start
there,hello
kenobi,general,thing!,other thing!
"#.to_owned(), *dbg!(buf_original.borrow()));
    }
}