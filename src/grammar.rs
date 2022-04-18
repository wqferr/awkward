use chumsky::prelude::*;
use std::collections::HashMap;
use regex::Regex;

use super::record::Record;
use super::types::{Value, Number, BuiltinFunction};

pub struct EvaluationContext {
    current_record: Option<Record>,
    ofs: String,
    field_names: HashMap<String, usize>,

    functions: HashMap<String, BuiltinFunction>,

    parsing_state: ParsingState
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ParsingState {
    Start,
    Processing,
    End
}

impl EvaluationContext {
    pub fn new(ofs: String) -> Self {
        Self {
            current_record: None,
            ofs,
            field_names: HashMap::new(),
            functions: HashMap::new(),

            parsing_state: ParsingState::Start
        }
    }

    pub fn set_header(&mut self, header: Record) {
        for (i, field_name) in header.into_iter().enumerate() {
            self.field_names.insert(field_name, i+1);
        }
    }

    pub fn register_builtin<F: 'static + FnMut(Vec<Value>) -> Value>(&mut self, name: &str, f: F) {
        self.functions.insert(name.to_owned(), Box::new(f));
    }

    pub fn set_current_record(&mut self, r: Record) {
        self.current_record = Some(r);
        self.parsing_state = ParsingState::Processing;
    }

    pub fn finish_parsing(&mut self) {
        self.current_record = None;
        self.parsing_state = ParsingState::End;
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    // These reference field index with their values
    // StrVar(usize),
    // NumVar(usize),
    // BoolVar?

    StrVar(FieldId),
    NumVar(FieldId),
    // BoolVar?

    StrLiteral(String),
    NumLiteral(Number),
    BoolLiteral(bool),

    FnCall(String, Vec<Expr>),

    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Concat(Box<Expr>, Box<Expr>),
    ConcatWSep(Box<Expr>, Box<Expr>),

    Eq(Box<Expr>, Box<Expr>),
    Ineq(Box<Expr>, Box<Expr>),
    Greater(Box<Expr>, Box<Expr>),
    GreaterEq(Box<Expr>, Box<Expr>),
    Lesser(Box<Expr>, Box<Expr>),
    LesserEq(Box<Expr>, Box<Expr>),

    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Not(Box<Expr>),

    Start,
    HasRecord,
    End,
    RegexSearch{ re: Regex, field: FieldId }
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pattern: Expr,
    actions: Vec<Expr>
}

impl Declaration {
    pub fn new(pattern: Expr, actions: Vec<Expr>) -> Self {
        Self { pattern, actions }
    }

    pub fn applies(&self, ctx: &mut EvaluationContext) -> bool {
        self.pattern.eval_bool(ctx)
    }

    pub fn execute_if_applies(&self, ctx: &mut EvaluationContext) {
        if self.applies(ctx) {
            for action in self.actions.iter() {
                action.eval(ctx);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum FieldId {
    Name(String),
    Idx(usize)
}

impl Expr {
    pub fn eval(&self, ctx: &mut EvaluationContext) -> Value {
        use Expr::*;
        match self {
            StrVar(field_id) => {
                let idx = match field_id {
                    FieldId::Name(name) => ctx.field_names[name],
                    FieldId::Idx(idx) => *idx,
                };
                if idx == 0 {
                    let mut buf = Box::new(Vec::new());
                    ctx.current_record.as_ref().unwrap().write(&mut buf, ctx.ofs.as_str()).unwrap();
                    Value::Str(String::from_utf8(*buf).unwrap())
                } else {
                    Value::Str(ctx.current_record.as_ref().unwrap().nth_str(idx).unwrap_or("").to_owned())
                }
            },
            NumVar(field_id) => {
                let idx = match field_id {
                    FieldId::Name(name) => ctx.field_names[name],
                    FieldId::Idx(idx) => *idx,
                };
                Value::Num(ctx.current_record.as_ref().unwrap().nth_num(idx).unwrap().to_owned())
            },
    
            StrLiteral(s) => Value::Str(s.clone()),
            NumLiteral(n) => Value::Num(n.clone()),
            BoolLiteral(b) => Value::Bool(*b),
    
            Neg(x) => Value::Num(-x.eval_num(ctx)),
            Add(x, y) => Value::Num(x.eval_num(ctx) + y.eval_num(ctx)),
            Sub(x, y) => Value::Num(x.eval_num(ctx) - y.eval_num(ctx)),
            Mul(x, y) => Value::Num(x.eval_num(ctx) * y.eval_num(ctx)),
            Div(x, y) => Value::Num(x.eval_num(ctx) / y.eval_num(ctx)),
            Mod(x, y) => Value::Num(x.eval_num(ctx) % y.eval_num(ctx)),
            Concat(x, y) => {
                let mut s1 = x.eval_str(ctx);
                let s2 = y.eval_str(ctx);
                s1.push_str(s2.as_str());
    
                Value::Str(s1)
            },
            ConcatWSep(x, y) => {
                let mut s1 = x.eval_str(ctx);
                let s2 = y.eval_str(ctx);
                s1.push_str(ctx.ofs.as_str());
                s1.push_str(s2.as_str());
    
                Value::Str(s1)
            },
    
            Eq(x, y) => Value::Bool(x.eval(ctx) == y.eval(ctx)),
            Ineq(x, y) => Value::Bool(x.eval(ctx) != y.eval(ctx)),
            Greater(x, y) => Value::Bool(x.eval_num(ctx) > y.eval_num(ctx)),
            GreaterEq(x, y) => Value::Bool(x.eval_num(ctx) >= y.eval_num(ctx)),
            Lesser(x, y) => Value::Bool(x.eval_num(ctx) < y.eval_num(ctx)),
            LesserEq(x, y) => Value::Bool(x.eval_num(ctx) <= y.eval_num(ctx)),
    
            And(x, y) => Value::Bool(x.eval_bool(ctx) && y.eval_bool(ctx)),
            Or(x, y) => Value::Bool(x.eval_bool(ctx) || y.eval_bool(ctx)),
            Not(x) => Value::Bool(!x.eval_bool(ctx)),
            FnCall(fname, args) => {
                // TODO check if function exists
                let mut vargs = vec![];
                for a in args {
                    vargs.push(a.eval(ctx));
                }
                let f = ctx.functions.get_mut(fname).unwrap();
                f(vargs)
            },
    
            Start => Value::Bool(ctx.parsing_state == ParsingState::Start),
            HasRecord => Value::Bool(ctx.parsing_state == ParsingState::Processing),
            End => Value::Bool(ctx.parsing_state == ParsingState::End),
            RegexSearch { re, field } => {
                let field_idx = match field {
                    FieldId::Name(name) => ctx.field_names[name],
                    FieldId::Idx(idx) => *idx,
                };
                let r = ctx.current_record.as_ref().unwrap();
                let s = if field_idx == 0 {
                    // search whole record if field is 0
                    r.original_string()
                } else {
                    r.field(field_idx)
                };
                Value::Bool(
                    re.find(s).is_some()
                )
            }
        }
    }
    
    pub fn eval_num(&self, ctx: &mut EvaluationContext) -> Number {
        match self.eval(ctx) {
            Value::Num(x) => x,
            Value::Str(s) => panic!("Expected number, found string `{}`", s),
            Value::Bool(b) => panic!("Expected number, found bool `{}`", b)
        }
    }
    
    pub fn eval_bool(&self, ctx: &mut EvaluationContext) -> bool {
        match self.eval(ctx) {
            Value::Bool(b) => b,
            Value::Str(s) => panic!("Expected bool, found string `{}`", s),
            Value::Num(x) => panic!("Expected bool, found number `{}`", x)
        }
    }
    
    pub fn eval_str(&self, ctx: &mut EvaluationContext) -> String {
        match self.eval(ctx) {
            Value::Str(s) => s,
            Value::Bool(b) => if b {"true".to_owned()} else {"false".to_owned()},
            Value::Num(x) => format!("{}", x)
        }
    }
}

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
            Declaration {
                // if pattern is omitted, execute for every record after it's started
                pattern: pat.unwrap_or(Expr::HasRecord),
                actions
            }
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
        ctx.set_header(Record::from("first,second,third".to_owned(), ","));
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
            put(@2), put(@1);
            start -> putline("start");
            end -> putline("end");
            (nr() % 2 == 0 && nr() > 0) -> put("thing!"), put("other thing!");
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

        for d in decl.iter() {
            d.execute_if_applies(&mut ctx);
        }

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

        (*record_number_original.borrow_mut()) = 0;
        (*line_is_empty_original.borrow_mut()) = true;
        ctx.finish_parsing();
        for d in decl.iter() {
            d.execute_if_applies(&mut ctx);
        }

        assert_eq!(r#"start
there,hello
kenobi,general,thing!,other thing!
end
"#.to_owned(), *dbg!(buf_original.borrow()));
    }
}