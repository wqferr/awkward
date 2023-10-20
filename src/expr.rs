use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;
use std::fmt::Display;
use regex::Regex;
use crate::record::Record;
use crate::types::{Value, Number, BuiltinFunction};

#[derive(Clone, Debug)]
pub enum Expr {
    StrField(FieldId),
    NumField(FieldId),
    BoolField(FieldId),
    FieldAssign(FieldId, Box<Expr>),

    Var(String),
    VarAssign(String, Box<Expr>),

    Delete(Vec<FieldId>),
    Keep(Vec<FieldId>),

    StrLiteral(String),
    NumLiteral(Number),
    BoolLiteral(bool),

    FnCall{ name: String, args: Vec<Expr> },

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

    RegexSearch{ re: Regex, field: FieldId }
}

pub struct EvaluationContext {
    current_record: Rc<RefCell<Record>>,
    ofs: String,
    initial_field_names: Rc<RefCell<HashMap<String, usize>>>,

    functions: HashMap<String, BuiltinFunction>,
    variables: Rc<RefCell<HashMap<String, Value>>>
}

#[derive(Debug, Clone)]
pub enum RuleCondition {
    Expression(Expr),
    Else,
    Start,
    End
}

#[derive(Debug, Clone)]
pub struct Rule {
    condition: RuleCondition,
    actions: Vec<Expr>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FieldId {
    Name(String),
    Idx(usize)
}

impl Display for FieldId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FieldId::Name(name) => write!(f, "\"{}\"", name),
            FieldId::Idx(idx) => write!(f, "{}", idx)
        }
    }
}

impl Expr {
    pub fn eval(&self, ctx: &mut EvaluationContext) -> Value {
        use Expr::*;
        match self {
            StrField(field_id) => {
                Value::Str(
                    ctx.current_record.borrow()
                        .get_possibly_entire_record(field_id, ctx.ofs.as_str())
                        .to_string())
            },
            NumField(field_id) => {
                Value::Num(
                    ctx.current_record.borrow().num_at(field_id).unwrap()
                )
            },
            BoolField(field_id) => {
                Value::Bool(
                    ctx.current_record.borrow().bool_at(field_id).unwrap()
                )
            },

            FieldAssign(id, expr) => {
                let result = expr.eval(ctx);
                let idx = match id {
                    FieldId::Idx(idx) => idx.clone(),
                    FieldId::Name(name) => ctx.initial_field_names.borrow()[name].clone(),
                };
                ctx.current_record
                    .borrow_mut()
                    .set(&FieldId::Idx(idx), result.to_string());
                result
            },

            Var(name) => {
                ctx.variables
                    .borrow()
                    .get(name)
                    .unwrap_or(&Value::Str(String::from("")))
                    .clone()
            },
            VarAssign(name, expr) => {
                let result = expr.eval(ctx);
                ctx.set_var(name.clone(), result.clone());
                result
            },

            Delete(ids) => {
                ctx.current_record.borrow_mut().drop_all(ids.as_slice());
                Value::Bool(true)
            },

            Keep(ids) => {
                ctx.current_record.borrow_mut().drop_all_but(ids.as_slice());
                Value::Bool(true)
            },

            StrLiteral(s) => Value::Str(s.clone()),
            NumLiteral(n) => Value::Num(*n),
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
                s1.push_str(&s2);

                Value::Str(s1)
            },
            ConcatWSep(x, y) => {
                let mut s1 = x.eval_str(ctx);
                let s2 = y.eval_str(ctx);
                s1.push_str(&ctx.ofs);
                s1.push_str(&s2);

                Value::Str(s1)
            },

            Eq(x, y) => Value::Bool(x.eval(ctx) == y.eval(ctx)),
            Ineq(x, y) => Value::Bool(x.eval(ctx) != y.eval(ctx)),
            Greater(x, y) => Value::Bool(x.eval_num(ctx) > y.eval_num(ctx)),
            GreaterEq(x, y) => Value::Bool(x.eval_num(ctx) >= y.eval_num(ctx)),
            Lesser(x, y) => Value::Bool(x.eval_num(ctx) < y.eval_num(ctx)),
            LesserEq(x, y) => Value::Bool(x.eval_num(ctx) <= y.eval_num(ctx)),

            And(x, y) => Value::Bool(x.eval_bool(ctx) && y.eval_bool(ctx)),
            Or(x, y)=> Value::Bool(x.eval_bool(ctx) || y.eval_bool(ctx)),
            Not(x) => Value::Bool(!x.eval_bool(ctx)),
            FnCall { name, args } => {
                let mut vargs = vec![];
                for a in args {
                    vargs.push(a.eval(ctx));
                }
                if !ctx.functions.contains_key(name) {
                    panic!("unknown function: {}", name);
                }
                let f = ctx.functions.get_mut(name).unwrap();
                f(vargs)
            },
            RegexSearch { re, field } => {
                let field_idx = match field {
                    FieldId::Name(name) => ctx.initial_field_names.borrow()[name],
                    FieldId::Idx(idx) => *idx,
                };
                let r = ctx.current_record.borrow();
                let s = if field_idx == 0 {
                    // search whole record if field is 0
                    r.original_string()
                } else {
                    r.get(&FieldId::Idx(field_idx))
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
        self.eval(ctx).is_truthy()
    }

    pub fn eval_str(&self, ctx: &mut EvaluationContext) -> String {
        format!("{}", self.eval(ctx))
    }
}

impl Rule {
    pub fn new(condition: RuleCondition, actions: Vec<Expr>) -> Self {
        Self { condition, actions }
    }

    pub fn applies(&self, last_condition_matched: &bool, ctx: &mut EvaluationContext) -> bool {
        use RuleCondition::*;
        match &self.condition {
            Else => !last_condition_matched,
            Expression(expr) => expr.eval_bool(ctx),
            Start | End => false
        }
    }

    pub fn condition(&self) -> &RuleCondition {
        &self.condition
    }

    pub fn execute(&self, ctx: &mut EvaluationContext) {
        for action in self.actions.iter() {
            action.eval(ctx);
        }
    }

    pub fn execute_if_applies(&self, last_condition_matched: &mut bool, ctx: &mut EvaluationContext) {
        if self.applies(last_condition_matched, ctx) {
            self.execute(ctx);
            *last_condition_matched = true;
        } else {
            *last_condition_matched = false;
        }
    }
}

impl EvaluationContext {
    pub fn new(ofs: String) -> Self {
        Self {
            current_record: Rc::new(RefCell::new(Record::empty())),
            ofs,
            initial_field_names: Rc::new(RefCell::new(HashMap::new())),

            functions: HashMap::new(),
            variables: Rc::new(RefCell::new(HashMap::new()))
        }
    }

    pub fn reset_field_names(&mut self) {
        for (k, v) in self.initial_field_names.borrow().iter() {
            self.current_record().borrow_mut().set_field_name(k.clone(), *v);
        }
    }

    pub fn set_field_names(&mut self, header: Vec<String>) {
        for (i, field_name) in header.into_iter().enumerate() {
            if field_name.len() > 0 {
                self.initial_field_names.borrow_mut().insert(field_name, i+1);
            }
        }
    }

    pub fn register_builtin<F: 'static + FnMut(Vec<Value>) -> Value>(&mut self, name: &str, f: F) {
        self.functions.insert(name.to_owned(), Box::new(f));
    }

    pub fn set_var(&mut self, name: String, v: Value) {
        self.variables.borrow_mut().insert(name, v);
    }

    pub fn set_current_record(&mut self, r: Record) {
        self.current_record.replace(r);
    }

    pub fn current_record(&self) -> &Rc<RefCell<Record>> {
        &self.current_record
    }

    pub fn ofs(&self) -> &str {
        self.ofs.as_str()
    }

    pub fn variables(&self) -> &RefCell<HashMap<String, Value>> {
        &self.variables
    }
}